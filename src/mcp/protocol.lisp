(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

;;; JSON-RPC & MCP Utilities

(defvar *stdout-lock* (bt:make-lock "stdout-lock")
  "Mutex serializing output to *standard-output* across concurrent worker threads.")

(defun to-list (val)
  "Ensure val is a list, converting from vector if necessary (preserving strings)."
  (cond
    ((stringp val) val)
    ((vectorp val) (coerce val 'list))
    (t val)))

(defun send-json (object)
  "Encode and send JSON over stdout under *stdout-lock*."
  (bt:with-lock-held (*stdout-lock*)
    (yason:encode object *standard-output*)
    (terpri *standard-output*)
    (force-output *standard-output*)))

(defun send-error (id code message)
  (send-json
    (dict "jsonrpc" "2.0" "id" id "error" (dict "code" code "message" message))))

(defun send-result (id result)
  (send-json (dict "jsonrpc" "2.0" "id" id "result" result)))

;;; Worker Thread Pool

(defvar *worker-queue-tail* nil)

(defvar *worker-queue* nil)
(defvar *worker-queue-lock* (bt:make-lock "worker-queue-lock"))
(defvar *worker-queue-cvar* (bt:make-condition-variable :name "worker-queue-cvar"))
(defvar *worker-threads* nil)
(defvar *worker-pool-running* nil)
(defparameter *default-worker-count* 4)

(defun process-worker-task (task)
  "Execute a queued worker task under a safe error handler."
  (when task
    (handler-case (funcall task)
      (serious-condition (e)
        (format *error-output* "~&[Worker Error] ~A~%" e)
        (force-output *error-output*)))))

(defun worker-loop ()
  (loop
    (let ((task nil))
      (bt:with-lock-held (*worker-queue-lock*)
                         (loop while (and *worker-pool-running* (null *worker-queue*))
                               do (bt:condition-wait *worker-queue-cvar* *worker-queue-lock*))
                         (when (null *worker-queue*) (unless *worker-pool-running* (return)))
                         (setf task (pop *worker-queue*))
                         (when (null *worker-queue*) (setf *worker-queue-tail* nil)))
      (process-worker-task task))))

(defun start-worker-pool
       (&optional (num-workers *default-worker-count*))
  "Initialize and start the worker thread pool for parallel JSON-RPC request processing."
  (bt:with-lock-held (*worker-queue-lock*)
                     (setf *worker-queue* nil *worker-queue-tail* nil *worker-pool-running* t))
  (setf *worker-threads*
        (loop repeat num-workers
              collect
              (bt:make-thread
                (lambda ()
                  (worker-loop))
                :name
                "mcp-worker-thread"))))

(defun stop-worker-pool ()
  "Stop all worker threads in the worker pool."
  (bt:with-lock-held (*worker-queue-lock*)
                     (setf *worker-pool-running* nil)
                     (loop repeat (* 2 (max 1 (length *worker-threads*)))
                           do (bt:condition-notify *worker-queue-cvar*)))
  (dolist (th *worker-threads*) (ignore-errors (bt:join-thread th)))
  (setf *worker-threads* nil))

(defun enqueue-task (task-thunk)
  "Enqueue TASK-THUNK for processing by the worker pool."
  (let ((cell (list task-thunk)))
    (bt:with-lock-held (*worker-queue-lock*)
                       (if *worker-queue-tail*
                         (setf (cdr *worker-queue-tail*) cell *worker-queue-tail* cell)
                         (setf *worker-queue* cell *worker-queue-tail* cell))
                       (bt:condition-notify *worker-queue-cvar*))))