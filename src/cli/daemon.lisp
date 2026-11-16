(in-package :structural-editing-mcp.cli)

#+(and sbcl (not win32))
(eval-when
    (:compile-toplevel :load-toplevel :execute)
  (require :sb-bsd-sockets))

(declaim (optimize (speed 2) (safety 3)))

(defvar *daemon-lock*
  (bt:make-lock "semcp-daemon-lock")
  "Serializes CLI requests handled in RAM by the daemon.")

(defun daemon-socket-path (session-dir)
  "Return pathname for daemon.sock inside SESSION-DIR."
  (merge-pathnames "daemon.sock" (uiop:ensure-directory-pathname session-dir)))

(defun daemon-pid-path (session-dir)
  "Return pathname for daemon.pid inside SESSION-DIR."
  (merge-pathnames "daemon.pid" (uiop:ensure-directory-pathname session-dir)))

(defun current-pid ()
  "Return current OS process ID as an integer."
  #+
  (and sbcl (not win32))
  (sb-unix:unix-getpid)
  #-
  (and sbcl (not win32))
  0)

(defun find-daemon-session-dir (flags)
  "Find .semcp directory containing daemon.sock unless --no-session or --no-daemon."
  (unless
      (or (flag-bool flags "no-session") (flag-bool flags "no-daemon"))
    (let*
        ((explicit (or (flag-get flags "session") (flag-get flags "state-dir")))
         (dir
           (if explicit
             (uiop:ensure-directory-pathname explicit)
             (or (find-session-dir)
                 (let
                     ((cand
                        (merge-pathnames (concatenate 'string *session-dir-name* "/") (uiop:getcwd))))
                   (when (probe-file (daemon-socket-path cand)) cand))))))
      (when (and dir (probe-file (daemon-socket-path dir))) dir))))

(defun daemon-send-request (sock-path req-dict)
  "Connect to local Unix socket SOCK-PATH, send REQ-DICT as JSON line, return parsed response dict or NIL."
  #+
  (and sbcl (not win32))
  (handler-case
      (let
          ((sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (unwind-protect
            (progn
              (sb-bsd-sockets:socket-connect sock (namestring sock-path))
              (let
                  ((stream
                     (sb-bsd-sockets:socket-make-stream sock
                                                        :input
                                                        t
                                                        :output
                                                        t
                                                        :element-type
                                                        'character
                                                        :external-format
                                                        :utf-8)))
                (write-string (json-encode-to-string req-dict) stream)
                (terpri stream)
                (finish-output stream)
                (let ((line (read-line stream nil nil)))
                  (when (and line (plusp (length line)))
                    (let ((yason:*parse-json-arrays-as-vectors* nil)) (yason:parse line))))))
          (ignore-errors (sb-bsd-sockets:socket-close sock))))
    (error () nil))
  #-
  (and sbcl (not win32))
  (progn (declare (ignore sock-path req-dict)) nil))

(defun daemon-ping (session-dir)
  "Ping the daemon for SESSION-DIR. Returns status dict or NIL if unreachable."
  (let
      ((sock-path (daemon-socket-path session-dir)))
    (when (probe-file sock-path)
      (daemon-send-request sock-path (dict "action" "ping")))))

(defun daemon-forward-cli (session-dir argv)
  "Forward ARGV to the running daemon for SESSION-DIR.
Returns (values output-string exit-code) or NIL if daemon is unreachable."
  (let
      ((sock-path (daemon-socket-path session-dir)))
    (when (probe-file sock-path)
      (let
          ((resp
             (daemon-send-request sock-path
                                  (dict "action" "cli" "argv" argv "cwd" (namestring (uiop:getcwd))))))
        (when (hash-table-p resp)
          (values (or (href resp "output") "") (or (href resp "exit_code") 0)))))))

(defun daemon-stats-dict ()
  "Return a summary dict of in-RAM workspaces and tracked files."
  (let ((ws-count 0) (file-count 0))
    (bt:with-lock-held (*workspace-registry-lock*)
                       (maphash
                         (lambda (id ws)
                           (declare (ignore id))
                           (incf ws-count)
                           (incf file-count (length (workspace-all-files-list ws))))
                         *workspace-registry*))
    (dict "ok" t "pid" (current-pid) "workspaces" ws-count "files" file-count)))

(defun handle-daemon-client (stream session-dir)
  "Handle a single JSON-line request on STREAM. Returns NIL to stop daemon, T to continue."
  (let
      ((line (read-line stream nil nil)) (keep-running t))
    (when (and line (plusp (length line)))
      (let*
          ((yason:*parse-json-arrays-as-vectors* nil)
           (req (handler-case (yason:parse line) (error () nil)))
           (action
             (if (hash-table-p req)
               (href req "action")
               ""))
           (resp
             (cond
               ((string= action "ping") (daemon-stats-dict))
               ((string= action "save")
                 (bt:with-lock-held (*daemon-lock*)
                                    (let ((sf (save-session session-dir)))
                                      (dict "ok" t "state_file" (namestring sf)))))
               ((string= action "stop")
                 (bt:with-lock-held (*daemon-lock*) (save-session session-dir))
                 (setf keep-running nil)
                 (dict "ok" t "stopped" t))
               ((string= action "cli")
                 (let
                     ((argv (href req "argv")) (cwd (href req "cwd")))
                   (bt:with-lock-held (*daemon-lock*)
                                      (multiple-value-bind (output exit-code)
                                                           (if (and cwd (probe-file cwd))
                                                             (uiop:with-current-directory (cwd) (execute-cli-in-ram argv session-dir))
                                                             (execute-cli-in-ram argv session-dir))
                                        (dict "ok" (zerop exit-code) "output" output "exit_code" exit-code)))))
               (t (dict "ok" yason:false "error" (fmt "Unknown daemon action: ~A" action))))))
        (write-string (json-encode-to-string resp) stream)
        (terpri stream)
        (finish-output stream)))
    keep-running))

(defun run-daemon-foreground
       (session-dir &key ready-hook)
  "Run the in-RAM workspace daemon on SESSION-DIR/daemon.sock until stopped."
  #+
  (and sbcl (not win32))
  (let*
      ((dir (uiop:ensure-directory-pathname session-dir))
       (sock-path (daemon-socket-path dir))
       (pid-path (daemon-pid-path dir))
       (state-file (session-state-file dir))
       (server-sock (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (ensure-directories-exist sock-path)
    (bt:with-lock-held (*daemon-lock*)
                       (if (probe-file state-file)
                         (load-session dir)
                         (reset-workspace-registry)))
    (when (probe-file sock-path) (ignore-errors (delete-file sock-path)))
    (unwind-protect
        (progn
          (sb-bsd-sockets:socket-bind server-sock (namestring sock-path))
          (sb-bsd-sockets:socket-listen server-sock 16)
          (with-open-file
              (out pid-path
                   :direction
                   :output
                   :if-exists
                   :supersede
                   :if-does-not-exist
                   :create)
            (format out "~D~%" (current-pid)))
          (when ready-hook (funcall ready-hook))
          (loop
            (let
                ((client (sb-bsd-sockets:socket-accept server-sock)))
              (let ((keep-running t))
                (unwind-protect
                    (let
                        ((stream
                           (sb-bsd-sockets:socket-make-stream client
                                                              :input
                                                              t
                                                              :output
                                                              t
                                                              :element-type
                                                              'character
                                                              :external-format
                                                              :utf-8)))
                      (setf keep-running (handle-daemon-client stream dir)))
                  (ignore-errors (sb-bsd-sockets:socket-close client)))
                (unless keep-running (return))))))
      (ignore-errors (sb-bsd-sockets:socket-close server-sock))
      (ignore-errors (delete-file sock-path))
      (ignore-errors (delete-file pid-path)))
    t)
  #-
  (and sbcl (not win32))
  (progn (declare (ignore session-dir ready-hook))
         (error "Daemon mode requires SBCL with Unix domain sockets.")))

(defun start-daemon-background (session-dir)
  "Launch the daemon in a detached background process and wait for it to become ready."
  (let*
      ((dir (uiop:ensure-directory-pathname session-dir))
       (exe (first (uiop:raw-command-line-arguments)))
       (cmd (list exe "daemon" "start" "--foreground" "--session" (namestring dir))))
    (uiop:launch-program cmd :input nil :output nil :error-output nil)
    (loop repeat 40
          for ping = (daemon-ping dir)
          when ping return ping
          do (sleep 0.05)
          finally
          (error
            "Timed out waiting for semcp daemon to start on ~A"
            (namestring (daemon-socket-path dir))))))