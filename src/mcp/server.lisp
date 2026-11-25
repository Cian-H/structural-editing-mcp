(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

(defun handle-message (msg)
  "Dispatch a parsed JSON-RPC message with per-request _meta version and capability binding."
  (let*
      ((jsonrpc (href msg "jsonrpc"))
       (id (href msg "id"))
       (method (href msg "method"))
       (params (href msg "params"))
       (meta (and (hash-table-p params) (href params "_meta")))
       (meta-ver
         (and (hash-table-p meta) (href meta "io.modelcontextprotocol/protocolVersion")))
       (meta-caps
         (and (hash-table-p meta)
              (href meta "io.modelcontextprotocol/clientCapabilities"))))
    (cond
      ((not (equal jsonrpc "2.0"))
        (when id
          (send-error id -32600 "Invalid Request: jsonrpc version must be \"2.0\"")))
      ((and (stringp meta-ver)
            (plusp (length meta-ver))
            (not (member meta-ver +supported-protocol-versions+ :test #'string=)))
        (when id
          (send-error-with-data id
                                +error-code-unsupported-protocol-version+
                                (fmt "Unsupported protocol version: ~A" meta-ver)
                                (dict "supportedVersions" +supported-protocol-versions+))))
      (t
        (let
            ((*request-protocol-version*
               (or
                 (and (stringp meta-ver) (plusp (length meta-ver)) meta-ver)
                 *negotiated-protocol-version*
                 +default-legacy-protocol-version+))
             (*request-client-capabilities* (or meta-caps *negotiated-client-capabilities*)))
          (string-case (or method "")
                       ("initialize" (handle-initialize id params))
                       ("server/discover" (handle-server-discover id params))
                       ("notifications/initialized" (setf *session-initialized-p* t) nil)
                       ("notifications/cancelled" nil)
                       ("ping" (when id (send-result id (dict))))
                       ("tools/list" (handle-tools-list id params))
                       ("tools/call" (handle-tools-call id params))
                       ("resources/list" (handle-resources-list id params))
                       ("resources/templates/list" (handle-resources-templates-list id params))
                       ("resources/read" (handle-resources-read id params))
                       ("resources/subscribe" (handle-resources-subscribe id params))
                       ("resources/unsubscribe" (handle-resources-unsubscribe id params))
                       ("resources/directory/read" (handle-resources-directory-read id params))
                       ("skills/list" (handle-skills-list id params))
                       ("skills/get" (handle-skills-get id params))
                       ("tasks/get" (handle-tasks-get id params))
                       ("tasks/list" (handle-tasks-list id params))
                       ("tasks/cancel" (handle-tasks-cancel id params))
                       ("tasks/update" (handle-tasks-update id params))
                       (t (when id (send-error id -32601 (fmt "Method not found: ~A" method))))))))))

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
                  (force-output *error-output*)
                  (send-error nil -32700 (fmt "Parse error: ~A" e))))))))
    (stop-worker-pool)))
