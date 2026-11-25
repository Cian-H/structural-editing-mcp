(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

;;; Resource Subscriptions & Providers

(defvar *resource-subscriptions*
  (make-hash-table :test #'equal)
  "Set of subscribed MCP resource URIs.")

(defvar *resource-subscriptions-lock*
  (bt:make-lock "resource-subscriptions-lock")
  "Lock protecting *RESOURCE-SUBSCRIPTIONS*.")

(defvar *extra-resource-listers* '()
                                 "List of zero-arg thunks returning extra MCP resource descriptor dicts.")

(defvar *extra-resource-readers* '()
                                 "List of functions (lambda (uri) ...) returning (values text mime-type) or NIL.")

(defun encode-json-to-string (obj)
  "Encode OBJ to a JSON string using yason."
  (yason:with-output-to-string* () (yason:encode obj)))

(defun workspace-status-uri (ws-id) (fmt "workspace://~A/status" ws-id))

(defun workspace-ast-uri (ws-id dialect filepath)
  (fmt "ast://~A/~A~A" ws-id (string-downcase (string dialect)) filepath))

(defun list-workspace-resources ()
  "Enumerate static/dynamic MCP resource descriptors for all active workspaces."
  (let ((entries '()))
    (dolist
        (ws-info (structural-editing-mcp.workspace:list-workspaces))
      (let*
          ((ws-id (getf ws-info :id))
           (ws (structural-editing-mcp.workspace:get-workspace ws-id :error-p nil)))
        (push
          (dict "uri"
                (workspace-status-uri ws-id)
                "name"
                (fmt "workspace-~A-status" ws-id)
                "description"
                (fmt "Live status, revision, and dirty/clean files for workspace ~S." ws-id)
                "mimeType"
                "application/json")
          entries)
        (when ws
          (dolist
              (filepath (structural-editing-mcp.workspace:workspace-all-files-list ws))
            (let
                ((dialect
                   (or (structural-editing-mcp.workspace:file-dialect filepath) :common-lisp)))
              (push
                (dict "uri"
                      (workspace-ast-uri ws-id dialect filepath)
                      "name"
                      (file-namestring filepath)
                      "description"
                      (fmt "In-memory AST rendering of ~A in workspace ~S." filepath ws-id)
                      "mimeType"
                      "text/x-lisp")
                entries))))))
    (dolist (lister *extra-resource-listers*)
      (setf entries (nconc (copy-list (funcall lister)) entries)))
    (nreverse entries)))

(defun parse-workspace-status-uri (uri)
  "Parse workspace://{id}/status, returning workspace ID or NIL."
  (when
      (serapeum:string-prefix-p "workspace://" uri)
    (let*
        ((rest (subseq uri (length "workspace://"))) (slash (position #\/ rest)))
      (when
          (and slash (string= (subseq rest (1+ slash)) "status"))
        (subseq rest 0 slash)))))

(defun parse-workspace-diff-uri (uri)
  "Parse workspace://{src}/diff/{tgt}, returning (values src tgt) or NIL."
  (when
      (serapeum:string-prefix-p "workspace://" uri)
    (let*
        ((rest (subseq uri (length "workspace://"))) (p1 (position #\/ rest)))
      (when p1
        (let*
            ((src (subseq rest 0 p1)) (after (subseq rest (1+ p1))))
          (when (serapeum:string-prefix-p "diff/" after)
            (let ((tgt (subseq after (length "diff/"))))
              (when (plusp (length tgt)) (values src tgt)))))))))

(defun parse-ast-uri (uri)
  "Parse ast://{ws-id}/{dialect}/{filepath}, returning (values ws-id dialect-kw filepath) or NIL."
  (when (serapeum:string-prefix-p "ast://" uri)
    (let*
        ((rest (subseq uri (length "ast://"))) (p1 (position #\/ rest)))
      (when p1
        (let*
            ((ws-id (subseq rest 0 p1))
             (after (subseq rest (1+ p1)))
             (p2 (position #\/ after)))
          (when p2
            (let
                ((dialect-str (subseq after 0 p2)) (filepath (subseq after p2)))
              (values ws-id (intern (string-upcase dialect-str) :keyword) filepath))))))))

(defun read-resource-uri (uri)
  "Resolve URI to (values text mime-type), or NIL if not found."
  (when-let
      ((ws-id (parse-workspace-status-uri uri)))
    (let
        ((ws (structural-editing-mcp.workspace:get-workspace ws-id :error-p nil)))
      (when ws
        (let*
            ((st (structural-editing-mcp.workspace:workspace-status ws))
             (json (encode-json-to-string (workspace-status-structured-dict st))))
          (return-from read-resource-uri (values json "application/json"))))))
  (multiple-value-bind (src tgt)
                       (parse-workspace-diff-uri uri)
    (when (and src tgt)
      (let
          ((s-ws (structural-editing-mcp.workspace:get-workspace src :error-p nil))
           (t-ws (structural-editing-mcp.workspace:get-workspace tgt :error-p nil)))
        (when (and s-ws t-ws)
          (let*
              ((df (structural-editing-mcp.workspace:diff-workspaces src tgt))
               (json (encode-json-to-string (workspace-diff-structured-dict df))))
            (return-from read-resource-uri (values json "application/json")))))))
  (multiple-value-bind (ws-id dialect filepath)
                       (parse-ast-uri uri)
    (when (and ws-id filepath)
      (let
          ((ws (structural-editing-mcp.workspace:get-workspace ws-id :error-p nil)))
        (when ws
          (let
              ((node
                 (structural-editing-mcp.workspace:with-workspace-context
                   (ws)
                   (let
                       ((coords (structural-editing-mcp.workspace::find-file-path-coords filepath)))
                     (when coords
                       (structural-editing-mcp.tree:get-node-at-path
                         structural-editing-mcp.workspace:*workspace-tree*
                         coords))))))
            (when node
              (return-from read-resource-uri
                           (values
                             (structural-editing-mcp.parser:sexp-to-string node :dialect dialect)
                             "text/x-lisp"))))))))
  (dolist (reader *extra-resource-readers*)
    (multiple-value-bind (text mime)
                         (funcall reader uri)
      (when text (return-from read-resource-uri (values text (or mime "text/plain"))))))
  nil)

(defun handle-resources-list (id params)
  "Handle MCP resources/list with optional cursor pagination."
  (let*
      ((all (list-workspace-resources))
       (cursor-str (and (hash-table-p params) (href params "cursor")))
       (offset
         (or (and (stringp cursor-str) (ignore-errors (parse-integer cursor-str))) 0))
       (limit 100)
       (total (length all))
       (start (min (max 0 offset) total))
       (end (min (+ start limit) total))
       (page (subseq all start end))
       (res (dict "resources" (coerce page 'vector))))
    (when (< end total) (setf (gethash "nextCursor" res) (write-to-string end)))
    (when (mcp-2-0-request-p)
      (setf (gethash "ttlMs" res)
            5000
            (gethash "cacheScope" res)
            "private"
            (gethash "resultType" res)
            "complete"))
    (send-result id res)))

(defun handle-resources-templates-list
       (id params)
  "Handle MCP resources/templates/list."
  (declare (ignore params))
  (let*
      ((templates
         (vector
           (dict "uriTemplate"
                 "workspace://{workspace_id}/status"
                 "name"
                 "Workspace Status"
                 "description"
                 "Live status, revision, and dirty/clean files for a workspace."
                 "mimeType"
                 "application/json")
           (dict "uriTemplate"
                 "workspace://{workspace_id}/diff/{target_id}"
                 "name"
                 "Workspace Diff"
                 "description"
                 "AST-level diff between two staged workspaces."
                 "mimeType"
                 "application/json")
           (dict "uriTemplate"
                 "ast://{workspace_id}/{dialect}/{filepath}"
                 "name"
                 "Staged AST File"
                 "description"
                 "In-memory rendered source of a file loaded in a workspace."
                 "mimeType"
                 "text/x-lisp")))
       (res (dict "resourceTemplates" templates)))
    (send-result id (maybe-wrap-mcp-2-0-result res))))

(defun handle-resources-read (id params)
  "Handle MCP resources/read, returning -32602 if the URI does not exist."
  (let
      ((uri (and (hash-table-p params) (href params "uri"))))
    (unless (and (stringp uri) (plusp (length uri)))
      (return-from handle-resources-read
                   (send-error id +error-code-invalid-params+ "Invalid params: uri is required")))
    (multiple-value-bind (text mime)
                         (read-resource-uri uri)
      (if (null text)
        (send-error-with-data id
                              +error-code-invalid-params+
                              (fmt "Resource not found: ~A" uri)
                              (dict "uri" uri))
        (let
            ((res (dict "contents" (vector (dict "uri" uri "mimeType" mime "text" text)))))
          (when (mcp-2-0-request-p)
            (setf (gethash "ttlMs" res)
                  5000
                  (gethash "cacheScope" res)
                  "private"
                  (gethash "resultType" res)
                  "complete"))
          (send-result id res))))))

(defun handle-resources-subscribe (id params)
  "Subscribe to updates for a resource URI."
  (let
      ((uri (and (hash-table-p params) (href params "uri"))))
    (unless (and (stringp uri) (plusp (length uri)))
      (return-from handle-resources-subscribe
                   (send-error id +error-code-invalid-params+ "Invalid params: uri is required")))
    (bt:with-lock-held
      (*resource-subscriptions-lock*)
      (setf (gethash uri *resource-subscriptions*) t))
    (let ((res (dict)))
      (when (mcp-2-0-request-p) (setf (gethash "resultType" res) "complete"))
      (send-result id res))))

(defun handle-resources-unsubscribe (id params)
  "Unsubscribe from updates for a resource URI."
  (let
      ((uri (and (hash-table-p params) (href params "uri"))))
    (unless (and (stringp uri) (plusp (length uri)))
      (return-from handle-resources-unsubscribe
                   (send-error id +error-code-invalid-params+ "Invalid params: uri is required")))
    (bt:with-lock-held
      (*resource-subscriptions-lock*)
      (remhash uri *resource-subscriptions*))
    (let ((res (dict)))
      (when (mcp-2-0-request-p) (setf (gethash "resultType" res) "complete"))
      (send-result id res))))

(defun notify-workspace-resources-changed
       (ws-id &key list-changed-p)
  "Emit notifications/resources/updated for any subscribed URIs in WS-ID."
  (let
      ((status-uri (workspace-status-uri ws-id))
       (prefix-ast (fmt "ast://~A/" ws-id))
       (prefix-diff (fmt "workspace://~A/diff/" ws-id))
       (matched '()))
    (bt:with-lock-held
      (*resource-subscriptions-lock*)
      (maphash
        (lambda (uri val)
          (declare (ignore val))
          (when
              (or (string= uri status-uri)
                  (serapeum:string-prefix-p prefix-ast uri)
                  (serapeum:string-prefix-p prefix-diff uri))
            (push uri matched)))
        *resource-subscriptions*))
    (dolist (uri matched)
      (send-notification "notifications/resources/updated" (dict "uri" uri)))
    (when
        (and list-changed-p *session-initialized-p*)
      (send-notification "notifications/resources/list_changed"))))