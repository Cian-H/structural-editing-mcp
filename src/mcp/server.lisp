(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

(defun handle-message (msg)
  "Dispatch a parsed JSON-RPC message."
  (let ((jsonrpc (href msg "jsonrpc"))
        (id (href msg "id"))
        (method (href msg "method"))
        (params (href msg "params")))
    (when (equal jsonrpc "2.0")
      (string-case (or method "")
        ("initialize" (handle-initialize id params))
        ("notifications/initialized" nil)
        ("notifications/cancelled" nil)
        ("ping" (when id (send-result id (dict))))
        ("tools/list" (handle-tools-list id params))
        ("tools/call" (handle-tools-call id params))
        (t (when id (send-error id -32601 (fmt "Method not found: ~A" method))))))))

(defun start-server ()
  "Start the MCP server loop over stdin/stdout with worker thread pool."
  (structural-editing-mcp.workspace:init-workspace)
  (start-worker-pool)
  (unwind-protect
      (let ((yason:*parse-json-arrays-as-vectors* nil))
        (loop
          (let ((line (read-line *standard-input* nil :eof)))
            (when (eq line :eof) (return))
            (when (plusp (length line))
              (handler-case
                  (let ((msg (yason:parse line)))
                    (enqueue-task
                      (lambda ()
                        (handle-message msg))))
                (error (e)
                  (format *error-output* "Parse error: ~A~%" e)
                  (force-output *error-output*)))))))
    (stop-worker-pool)))
