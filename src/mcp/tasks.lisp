(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

;;; Background Tasks (io.modelcontextprotocol/tasks, SEP-2663)

(defstruct
    (mcp-task (:constructor make-mcp-task-internal))
  "Represents a background MCP task execution."
  (id "" :type string)
  (status :working :type keyword)
  (tool-name "" :type string)
  (workspace-id "default" :type string)
  (created-at-ms 0 :type integer)
  (updated-at-ms 0 :type integer)
  (ttl-ms 300000 :type integer)
  (poll-interval-ms 100 :type integer)
  (result nil)
  (error-info nil)
  (input-requests nil)
  (continuation nil)
  (cancel-requested-p nil :type boolean))

(defvar *task-registry*
  (make-hash-table :test #'equal)
  "Registry mapping taskId strings to MCP-TASK structs.")

(defvar *task-registry-lock*
  (bt:make-lock "mcp-task-registry-lock")
  "Mutex protecting *TASK-REGISTRY*.")

(defvar *task-counter* 0 "Monotonic counter for generating unique task IDs.")

(defvar *current-task* nil
                       "Dynamically bound to the active MCP-TASK inside a background task worker.")

(defun current-time-ms ()
  "Return monotonic-like wall time in milliseconds."
  (truncate (* 1000 (/ (get-internal-real-time) internal-time-units-per-second))))

(defun task-terminal-p (status)
  (member status '(:completed :failed :cancelled)))

(defun prune-expired-tasks-unlocked (now-ms)
  "Remove terminal tasks whose TTL has elapsed (caller must hold *TASK-REGISTRY-LOCK*)."
  (let ((expired '()))
    (maphash
      (lambda (id task)
        (when
            (and (task-terminal-p (mcp-task-status task))
                 (> now-ms (+ (mcp-task-updated-at-ms task) (mcp-task-ttl-ms task))))
          (push id expired)))
      *task-registry*)
    (dolist (id expired) (remhash id *task-registry*))))

(defun check-task-cancelled ()
  "Signal an error if *CURRENT-TASK* has been cancelled."
  (when
      (and *current-task* (mcp-task-cancel-requested-p *current-task*))
    (error "Task ~A was cancelled." (mcp-task-id *current-task*))))

(defun task-status-string (status) (string-downcase (string status)))

(defun task-to-dict (task &key create-result-p)
  "Serialize MCP-TASK to a JSON-friendly dict."
  (let
      ((d
         (dict "taskId"
               (mcp-task-id task)
               "status"
               (task-status-string (mcp-task-status task))
               "ttlMs"
               (mcp-task-ttl-ms task)
               "pollIntervalMs"
               (mcp-task-poll-interval-ms task)
               "createdAtMs"
               (mcp-task-created-at-ms task)
               "lastUpdatedAtMs"
               (mcp-task-updated-at-ms task))))
    (when create-result-p (setf (gethash "resultType" d) "task"))
    (unless create-result-p
      (when (mcp-2-0-request-p) (setf (gethash "resultType" d) "complete"))
      (when
          (and (eq (mcp-task-status task) :completed) (mcp-task-result task))
        (setf (gethash "result" d) (mcp-task-result task)))
      (when
          (and (eq (mcp-task-status task) :failed) (mcp-task-error-info task))
        (setf (gethash "error" d) (mcp-task-error-info task)))
      (when
          (and (eq (mcp-task-status task) :input_required) (mcp-task-input-requests task))
        (setf (gethash "inputRequests" d) (mcp-task-input-requests task))))
    d))

(defun task-requested-p (params)
  "Return T when the caller requests task-augmented execution for tools/call."
  (let*
      ((meta (and (hash-table-p params) (href params "_meta")))
       (task-param (and (hash-table-p params) (href params "task")))
       (meta-task
         (and (hash-table-p meta)
              (or (href meta "io.modelcontextprotocol/task") (href meta "task")))))
    (or (hash-table-p task-param)
        (eq task-param t)
        (hash-table-p meta-task)
        (eq meta-task t))))

(defun extract-task-ttl-ms (params)
  (let*
      ((meta (and (hash-table-p params) (href params "_meta")))
       (task-obj
         (or
           (and (hash-table-p params)
                (hash-table-p (href params "task"))
                (href params "task"))
           (and (hash-table-p meta)
                (hash-table-p (href meta "io.modelcontextprotocol/task"))
                (href meta "io.modelcontextprotocol/task"))))
       (ttl (and task-obj (or (href task-obj "ttlMs") (href task-obj "ttl")))))
    (if (and (integerp ttl) (plusp ttl))
      (min ttl 3600000)
      300000)))

(defun run-in-task-worker (thunk)
  "Execute THUNK on the worker pool if running, otherwise on a background thread."
  (if *worker-pool-running*
    (enqueue-task thunk)
    (bt:make-thread thunk :name "mcp-task-worker")))

(defun create-and-start-tool-task
       (tool-name ws-id ttl-ms thunk)
  "Create a background MCP-TASK, schedule THUNK, and return the CreateTaskResult dict."
  (let*
      ((now (current-time-ms))
       (task
         (bt:with-lock-held (*task-registry-lock*)
                            (prune-expired-tasks-unlocked now)
                            (incf *task-counter*)
                            (let
                                ((t-obj
                                   (make-mcp-task-internal :id
                                                           (fmt "task-~D-~D" *task-counter* now)
                                                           :status
                                                           :working
                                                           :tool-name
                                                           tool-name
                                                           :workspace-id
                                                           ws-id
                                                           :created-at-ms
                                                           now
                                                           :updated-at-ms
                                                           now
                                                           :ttl-ms
                                                           ttl-ms)))
                              (setf (gethash (mcp-task-id t-obj) *task-registry*) t-obj)
                              t-obj))))
    (run-in-task-worker
      (lambda ()
        (let ((*current-task* task))
          (handler-case
              (multiple-value-bind (content structured)
                                   (funcall thunk)
                (bt:with-lock-held (*task-registry-lock*)
                                   (setf (mcp-task-updated-at-ms task) (current-time-ms))
                                   (cond
                                     ((mcp-task-cancel-requested-p task) (setf (mcp-task-status task) :cancelled))
                                     (t
                                       (let
                                           ((res (dict "content" (list (dict "type" "text" "text" content)))))
                                         (when structured (setf (gethash "structuredContent" res) structured))
                                         (setf (mcp-task-result task) res (mcp-task-status task) :completed))))))
            (error (e)
              (bt:with-lock-held (*task-registry-lock*)
                                 (setf (mcp-task-updated-at-ms task) (current-time-ms))
                                 (if (mcp-task-cancel-requested-p task)
                                   (setf (mcp-task-status task) :cancelled)
                                   (setf (mcp-task-status task)
                                         :failed
                                         (mcp-task-error-info task)
                                         (dict "code" +error-code-internal-error+ "message" (fmt "~A" e))))))))))
    (task-to-dict task :create-result-p t)))

(defun lookup-task-or-error (id params)
  (let*
      ((task-id (and (hash-table-p params) (href params "taskId")))
       (task
         (and (stringp task-id)
              (bt:with-lock-held (*task-registry-lock*)
                                 (prune-expired-tasks-unlocked (current-time-ms))
                                 (gethash task-id *task-registry*)))))
    (cond
      ((not (and (stringp task-id) (plusp (length task-id))))
        (send-error id +error-code-invalid-params+ "Invalid params: taskId is required")
        nil)
      ((null task)
        (send-error-with-data id
                              +error-code-invalid-params+
                              (fmt "Task not found: ~A" task-id)
                              (dict "taskId" task-id))
        nil)
      (t task))))

(defun handle-tasks-get (id params)
  "Handle tasks/get returning current status and inline result when terminal."
  (when-let
      ((task (lookup-task-or-error id params)))
    (let
        ((payload (bt:with-lock-held (*task-registry-lock*) (task-to-dict task))))
      (send-result id payload))))

(defun handle-tasks-list (id params)
  "Handle tasks/list returning active and non-expired tasks."
  (declare (ignore params))
  (let
      ((items
         (bt:with-lock-held (*task-registry-lock*)
                            (prune-expired-tasks-unlocked (current-time-ms))
                            (loop for task being the hash-values of *task-registry*
                                  collect (task-to-dict task)))))
    (let
        ((res (dict "tasks" (coerce items 'vector))))
      (when (mcp-2-0-request-p) (setf (gethash "resultType" res) "complete"))
      (send-result id res))))

(defun handle-tasks-cancel (id params)
  "Handle tasks/cancel cooperatively."
  (when-let
      ((task (lookup-task-or-error id params)))
    (let
        ((payload
           (bt:with-lock-held (*task-registry-lock*)
                              (setf (mcp-task-cancel-requested-p task)
                                    t
                                    (mcp-task-updated-at-ms task)
                                    (current-time-ms))
                              (unless (task-terminal-p (mcp-task-status task))
                                (setf (mcp-task-status task) :cancelled))
                              (task-to-dict task))))
      (send-result id payload))))

(defun handle-tasks-update (id params)
  "Handle tasks/update to supply inputResponses for an input_required task."
  (when-let
      ((task (lookup-task-or-error id params)))
    (let
        ((responses (and (hash-table-p params) (href params "inputResponses")))
         (cont (bt:with-lock-held (*task-registry-lock*) (mcp-task-continuation task))))
      (if
          (and (eq (mcp-task-status task) :input_required) cont)
        (progn
          (handler-case
              (multiple-value-bind (content structured)
                                   (funcall cont responses)
                (bt:with-lock-held (*task-registry-lock*)
                                   (let
                                       ((res (dict "content" (list (dict "type" "text" "text" content)))))
                                     (when structured (setf (gethash "structuredContent" res) structured))
                                     (setf (mcp-task-result task)
                                           res
                                           (mcp-task-status task)
                                           :completed
                                           (mcp-task-continuation task)
                                           nil
                                           (mcp-task-updated-at-ms task)
                                           (current-time-ms)))))
            (error (e)
              (bt:with-lock-held (*task-registry-lock*)
                                 (setf (mcp-task-status task)
                                       :failed
                                       (mcp-task-error-info task)
                                       (dict "code" +error-code-internal-error+ "message" (fmt "~A" e))
                                       (mcp-task-continuation task)
                                       nil
                                       (mcp-task-updated-at-ms task)
                                       (current-time-ms)))))
          (send-result id (bt:with-lock-held (*task-registry-lock*) (task-to-dict task))))
        (send-error id
                    +error-code-invalid-params+
                    "Task is not awaiting input_required.")))))