(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

(defparameter *delimiter-name-map*
  (dict "paren"
        :paren
        "()"
        :paren
        ""
        :paren
        "square"
        :square
        "bracket"
        :square
        "[]"
        :square
        "curly"
        :curly
        "brace"
        :curly
        "{}"
        :curly)
  "Map of wrapper aliases to AST delimiter keywords.")

(defun parse-delimiter-type (wrapper-str)
  "Map WRAPPER-STR to :paren, :square, :curly, or NIL if it represents a custom wrapper form."
  (let*
      ((clean-str (string-trim '(#\space #\tab #\newline #\:) (or wrapper-str "")))
       (lower (string-downcase clean-str)))
    (gethash lower *delimiter-name-map*)))

(defun wrap-node-with-custom-form
       (tree path wrapper-str)
  "Wrap node at PATH using the custom form expression in WRAPPER-STR."
  (let*
      ((parsed (structural-editing-mcp.parser:string-to-sexp wrapper-str))
       (expr (first (structural-editing-mcp.tree:get-node-children parsed))))
    (if
        (and expr (structural-editing-mcp.tree:get-node-children expr))
      (structural-editing-mcp.tree:update-node-at-path
        tree
        path
        (lambda (target-node)
          (match expr
                 ((node p tag children) ` (:path ,p ,tag ,@children ,target-node)))))
      (structural-editing-mcp.edit:wrap-node tree path :paren))))

(defun wrap-range-with-custom-form
       (tree parent-path start-idx end-index wrapper-str)
  "Wrap range of nodes under PARENT-PATH using custom form expression in WRAPPER-STR."
  (let*
      ((parsed (structural-editing-mcp.parser:string-to-sexp wrapper-str))
       (expr (first (structural-editing-mcp.tree:get-node-children parsed))))
    (if
        (and expr (structural-editing-mcp.tree:get-node-children expr))
      (structural-editing-mcp.tree:update-node-at-path
        tree
        parent-path
        (lambda (parent)
          (match parent
                 ((node p ptag children)
                  (match expr
                         ((node _ tag expr-children) `
                          (:path ,p
                                 ,ptag
                                 ,@
                                 (subseq children 0 start-idx)
                                 (:path ,p ,tag ,@expr-children ,@ (subseq children start-idx (1+ end-index)))
                                 ,@
                                 (subseq children (1+ end-index))))))
                 (_ parent))))
      (structural-editing-mcp.edit:wrap-range tree
                                              parent-path
                                              start-idx
                                              end-index
                                              :paren))))

(defun perform-wrap-single
       (tree path wrapper-str delim)
  "Wrap a single node at PATH with DELIM or custom WRAPPER-STR."
  (if delim
    (structural-editing-mcp.edit:wrap-node tree path delim)
    (wrap-node-with-custom-form tree path wrapper-str)))

(defun perform-wrap-range
       (tree parent-path start-idx end-index wrapper-str delim)
  "Wrap a range of nodes from START-IDX to END-INDEX under PARENT-PATH."
  (if delim
    (structural-editing-mcp.edit:wrap-range tree
                                            parent-path
                                            start-idx
                                            end-index
                                            delim)
    (wrap-range-with-custom-form tree parent-path start-idx end-index wrapper-str)))

(defun resolve-wrap-parent-and-start (path index)
  "Resolve parent path and starting index for a range wrap."
  (let
      ((parent-path
         (cond
           (index path)
           ((null (cdr path)) path)
           (t (butlast path))))
       (start-idx
         (cond
           (index index)
           ((null (cdr path)) 0)
           (t (lastcar path)))))
    (values parent-path start-idx)))

(defun perform-wrap
       (tree path wrapper-str &optional end-index index)
  "Wrap the node at PATH, or range of nodes from START-INDEX to END-INDEX under PARENT-PATH."
  (let ((delim (parse-delimiter-type wrapper-str)))
    (if (null end-index)
      (perform-wrap-single tree path wrapper-str delim)
      (multiple-value-bind (parent-path start-idx)
                           (resolve-wrap-parent-and-start path index)
        (perform-wrap-range tree parent-path start-idx end-index wrapper-str delim)))))

(defun resolve-parent-and-index
       (tree target-path &optional index)
  "Resolve target parent path and index for move or copy."
  (if index
    (values target-path index)
    (if (null (cdr target-path))
      (values target-path
              (length
                (structural-editing-mcp.tree:get-node-children
                  (structural-editing-mcp.tree:get-node-at-path tree target-path))))
      (values (butlast target-path) (lastcar target-path)))))

(defun resolve-split-index (tgt index)
  "Resolve split index from INDEX argument or last element of TGT."
  (or index
      (if (null (cdr tgt))
        0
        (lastcar tgt))))

(defun perform-relocate-action
       (action src tgt index tree)
  "Dispatch relocation mutation and return the updated tree."
  (string-case (or action "")
               (("move" "copy")
                (multiple-value-bind (tgt-parent tgt-idx)
                                     (resolve-parent-and-index tree tgt index)
                  (if (equal action "move")
                    (structural-editing-mcp.edit:move-node tree src tgt-parent tgt-idx)
                    (structural-editing-mcp.edit:copy-node tree src tgt-parent tgt-idx))))
               ("swap" (structural-editing-mcp.edit:swap-nodes tree src tgt))
               ("merge" (structural-editing-mcp.edit:merge-nodes tree src tgt))
               ("split"
                (structural-editing-mcp.edit:split-node tree
                                                        tgt
                                                        (resolve-split-index tgt index)))
               (t (error "Unknown action: ~A" action))))

(defun perform-insert
       (tree path new-node-str &optional index)
  "Insert NEW-NODE-STR into TREE. If INDEX is provided, PATH is the parent.
Otherwise, PATH specifies the target location (parent is (butlast path), index is (lastcar path))."
  (multiple-value-bind (parent-path target-idx)
                       (resolve-parent-and-index tree path index)
    (structural-editing-mcp.edit:insert-expression
      tree
      parent-path
      target-idx
      new-node-str)))

(defun format-mutation-result
       (message target-path tree)
  (let*
      ((preview-path
         (cond
           ((null target-path) nil)
           ((null (cdr target-path)) target-path)
           (t (butlast target-path))))
       (preview-node
         (and preview-path
              (structural-editing-mcp.tree:get-node-at-path tree preview-path))))
    (if preview-node
      (fmt "~A~%~%Updated preview at ~A:~%~A"
           message
           preview-path
           (format-node-preview preview-node :depth 2))
      message)))

(defun perform-search (tree target-path query)
  "Search the AST under TARGET-PATH for leaf nodes matching QUERY."
  (structural-editing-mcp.analysis:search-ast tree query :path target-path))

(defun perform-rename
       (tree target-path old-name new-name)
  "Recursively rename all leaf nodes matching OLD-NAME to NEW-NAME under TARGET-PATH."
  (let
      ((lower-old (string-downcase old-name))
       (parsed-new
         (first
           (structural-editing-mcp.tree:get-node-children
             (structural-editing-mcp.parser:string-to-sexp new-name)))))
    (labels
        ((walk (node)
           (match node
                  ((leaf path val)
                   (let*
                       ((str (structural-editing-mcp.parser::format-atom val))
                        (lower-str (string-downcase str)))
                     (if (string= lower-old lower-str)
                       (match parsed-new
                              ((node _ tag children) ` (:path ,path ,tag ,@children))
                              ((leaf _ new-val) ` (:path ,path :leaf ,new-val))
                              (_ node))
                       node)))
                  ((node path tag children) (list* :path path tag (mapcar #'walk children)))
                  (_ node))))
      (if target-path
        (structural-editing-mcp.tree:update-node-at-path tree target-path #'walk)
        (walk tree)))))

;;; Tool Schema Builders & Definitions

(defun prop-path
       (&optional (desc "0-indexed array of integers specifying the AST path."))
  (dict "type" "array" "items" (dict "type" "integer") "description" desc))

(defun prop-string (desc &key enum)
  (let
      ((d (dict "type" "string" "description" desc)))
    (when enum (setf (gethash "enum" d) enum))
    d))

(defun prop-integer (desc) (dict "type" "integer" "description" desc))

(defun prop-boolean (desc) (dict "type" "boolean" "description" desc))

(defun prop-string-array (desc)
  (dict "type" "array" "items" (dict "type" "string") "description" desc))

(defun tool-schema (props &optional required)
  (let
      ((s (dict "type" "object" "properties" props)))
    (when required (setf (gethash "required" s) required))
    s))

(defun make-tool
       (name desc props &optional required)
  (dict "name" name "description" desc "inputSchema" (tool-schema props required)))

(defparameter *mutation-tools* '
  ("ast_modify" "ast_remove"
   "ast_relocate"
   "ast_rename"
   "ast_replace_pattern"
   "ast_extract_variable"
   "ast_extract_function")
  "List of tool names that mutate the AST.")

(defun mutation-tool-p (name)
  "Return T if tool NAME mutates workspace AST state."
  (member name *mutation-tools* :test #'string=))

(defvar *mcp-tool-handlers*
  (make-hash-table :test #'equal)
  "Registry mapping MCP tool name to handler function: (lambda (args &key path dialect agent-id) ...).")

(defvar *mcp-tool-definitions* '
  ()
  "List of tool definitions for tools/list in registration order.")

(defun register-tool-definition (tool-dict)
  "Register TOOL-DICT into *MCP-TOOL-DEFINITIONS*, preserving order without duplicates."
  (let ((name (gethash "name" tool-dict)))
    (setf *mcp-tool-definitions*
          (nconc
            (remove name
                    *mcp-tool-definitions*
                    :test
                    #'equal
                    :key
                    (lambda (d)
                      (gethash "name" d)))
            (list tool-dict)))))

(defparameter +prop-path+
  (prop-path
    "0-indexed array of integers specifying the AST path. Omit or pass [] for the workspace root, [0] for dialect 0 (e.g. :common-lisp), [0, 0] for file 0 in dialect 0, [0, 0, 2] for top-level form 2 in file 0, [0, 0, 3, 1] for child 1 of form 3."))

(defparameter +prop-load-files+
  (prop-string-array
    "Optional list of file or directory paths to load into the workspace (e.g. ['/path/to/project'] or ['/path/to/file.lisp']). Directories are recursively scanned for Lisp source files."))

(defparameter +prop-dialect+
  (prop-string
    "Optional Lisp dialect override (:common-lisp, :clojure, :scheme, :emacs-lisp, :fennel). Inferred if omitted."))

(defparameter +prop-agent-id+
  (prop-string
    "Optional identifier for the calling agent (e.g. 'agent-1', 'refactorer'). Used for automatic multi-agent concurrency tracking."))

(defparameter +prop-workspace-id+
  (prop-string
    "Target workspace identifier (default: 'default'). Use an isolated workspace ID (via workspace_manage 'fork') to branch and stage edits safely without clobbering other agents."))

(defun build-prop-schema (spec)
  "Build JSON schema property dict from parameter spec (pname &key type doc enum default items required)."
  (let*
      ((ptype (getf (cdr spec) :type :string))
       (pdoc (or (getf (cdr spec) :doc) (getf (cdr spec) :description) ""))
       (penum (getf (cdr spec) :enum)))
    (cond
      ((eq ptype :path)
        (if (and pdoc (plusp (length pdoc)))
          (prop-path pdoc)
          +prop-path+))
      ((eq ptype :load-files) +prop-load-files+)
      ((eq ptype :dialect) +prop-dialect+)
      ((eq ptype :workspace-id) +prop-workspace-id+)
      ((eq ptype :agent-id) +prop-agent-id+)
      ((eq ptype :string-array) (prop-string-array pdoc))
      ((eq ptype :integer) (prop-integer pdoc))
      ((eq ptype :boolean) (prop-boolean pdoc))
      ((eq ptype :string) (prop-string pdoc :enum penum))
      (t (dict "type" (string-downcase (string ptype)) "description" pdoc)))))

(defmacro define-mcp-tool
          (name (&key description mutation (workspace t)) params &body body)
  "Define an MCP tool, registering its JSON schema in *MCP-TOOL-DEFINITIONS*
and its execution handler in *MCP-TOOL-HANDLERS*."
  (let*
      ((handler-fn
         (intern
           (string-upcase (format nil "DISPATCH-MCP-TOOL-~A" (substitute #\- #\_ name)))))
       (effective-params
         (if workspace
           (let ((res (copy-list params)))
             (unless (member 'workspace_id res :key #'car)
               (setf res (append res '((workspace_id :type :workspace-id)))))
             (unless (member 'agent_id res :key #'car)
               (setf res (append res '((agent_id :type :agent-id)))))
             res)
           params))
       (props-var (gensym "PROPS"))
       (reqs-var (gensym "REQS")))
    `
    (progn
      (let
          ((,props-var (make-hash-table :test #'equal)) (,reqs-var '()))
        ,@
        (loop for p in effective-params for pname =
              (first p) for key-str =
              (string-downcase (substitute #\_ #\- (string pname)))
              for req-p =
              (getf (cdr p) :required) collect `
              (setf (gethash ,key-str ,props-var) (build-prop-schema ',p))
              when req-p collect `
              (push ,key-str ,reqs-var))
        (register-tool-definition
          (make-tool ,name ,description ,props-var (nreverse ,reqs-var))))
      (defun ,handler-fn
             (args &key (path nil) (dialect nil) (agent-id "default") &allow-other-keys)
        (declare (ignorable args path dialect agent-id))
        (let*
            ((path (or path (to-list (gethash "path" args))))
             (dialect (or dialect (parse-dialect-arg (gethash "dialect" args))))
             (agent-id
               (or (gethash "agent_id" args) (gethash "agent-id" args) agent-id "default"))
             ,@
             (loop for p in effective-params for pname =
                   (first p) unless
                   (member pname '(path dialect agent-id agent_id))
                   collect
                   (let
                       ((key-str (string-downcase (substitute #\_ #\- (string pname))))
                        (default (getf (cdr p) :default)))
                     (if default
                       `
                       (,pname (or (gethash ,key-str args) ,default))
                       `
                       (,pname (gethash ,key-str args))))))
          (declare (ignorable path dialect agent-id ,@ (mapcar #'first effective-params)))
          ,@body))
      (setf (gethash ,name *mcp-tool-handlers*) #',handler-fn)
      ,@
      (when mutation
        `
        ((unless
             (member ,name *mutation-tools* :test #'string=)
           (setf *mutation-tools* (append *mutation-tools* (list ,name))))))
      ',handler-fn)))

(defun get-tools-list () *mcp-tool-definitions*)

(defun handle-initialize (id params)
  (declare (ignore params))
  (send-result id
               (dict "protocolVersion"
                     "2024-11-05"
                     "capabilities"
                     (dict "tools" (make-hash-table))
                     "serverInfo"
                     (dict "name" "structural-editing-mcp" "version" +version+))))

(defun handle-tools-list (id params)
  (declare (ignore params))
  (send-result id (dict "tools" (get-tools-list))))

(defun parse-dialect-arg (dialect-str)
  "Parse an optional dialect string (e.g. \":clojure\" or \"common-lisp\") into a keyword."
  (when
      (and dialect-str (plusp (length dialect-str)))
    (intern (string-upcase (string-left-trim ":" dialect-str)) :keyword)))

;;; Tool Analysis & Workspace Execution Helpers

(defun run-tool-lint (tree path dialect args)
  "Run AST linter and return formatted findings."
  (let*
      ((rules (to-list (gethash "rules" args)))
       (findings
         (structural-editing-mcp.analysis:lint-ast tree
                                                   :path
                                                   path
                                                   :dialect
                                                   dialect
                                                   :rules
                                                   rules)))
    (structural-editing-mcp.analysis:format-lint-findings findings)))

(defun run-tool-complexity
       (tree path dialect args)
  "Run cyclomatic complexity analysis and return report."
  (let*
      ((min-cc (or (gethash "min_complexity" args) 1))
       (min-d (or (gethash "min_depth" args) 1))
       (results
         (structural-editing-mcp.analysis:analyze-complexity
           tree
           :path
           path
           :dialect
           dialect
           :min-complexity
           min-cc
           :min-depth
           min-d)))
    (structural-editing-mcp.analysis:format-complexity-report results)))

(defun run-tool-duplicates (tree path args)
  "Run duplicate subtree detector and return formatted report."
  (let*
      ((min-nodes (or (gethash "min_nodes" args) 4))
       (min-depth (or (gethash "min_depth" args) 2))
       (exact
         (let ((val (gethash "exact" args)))
           (if (null val)
             t
             val)))
       (results
         (structural-editing-mcp.analysis:find-duplicate-subtrees
           tree
           :path
           path
           :min-nodes
           min-nodes
           :min-depth
           min-depth
           :exact
           exact)))
    (structural-editing-mcp.analysis:format-duplicate-report results)))

(defun run-tool-bindings (tree path dialect args)
  "Run variable binding analysis and return formatted report."
  (let*
      ((inc-unused
         (let ((val (gethash "include_unused" args)))
           (if (null val)
             t
             val)))
       (inc-shadowed
         (let ((val (gethash "include_shadowed" args)))
           (if (null val)
             t
             val)))
       (findings
         (structural-editing-mcp.analysis:analyze-bindings
           tree
           :path
           path
           :include-unused
           inc-unused
           :include-shadowed
           inc-shadowed
           :dialect
           dialect)))
    (structural-editing-mcp.analysis:format-binding-report findings)))

(defun run-tool-suggestions
       (tree path dialect args)
  "Aggregate analysis findings into prioritized suggestions."
  (let*
      ((min-p (or (gethash "min_priority" args) "low"))
       (cats (to-list (gethash "categories" args)))
       (suggestions
         (structural-editing-mcp.analysis:suggest-refactorings
           tree
           :path
           path
           :min-priority
           min-p
           :categories
           cats
           :dialect
           dialect)))
    (structural-editing-mcp.analysis:format-refactoring-suggestions suggestions)))

(defun manage-snapshot-restore
       (action ws-id snap-name)
  "Handle snapshot and restore actions for workspace WS-ID."
  (let
      ((ws (structural-editing-mcp.workspace:get-workspace ws-id)))
    (cond
      ((equal action "snapshot")
        (structural-editing-mcp.workspace:snapshot-workspace snap-name ws)
        (fmt "Snapshot ~S created for workspace ~S." snap-name ws-id))
      ((equal action "restore")
        (structural-editing-mcp.workspace:restore-workspace snap-name ws)
        (fmt "Workspace ~S restored from snapshot ~S." ws-id snap-name)))))

(defun manage-create-file (args ws-id)
  "Create or add a file in the given workspace."
  (let*
      ((filepath
         (or (gethash "filepath" args)
             (gethash "file_path" args)
             (gethash "file" args)
             (gethash "path" args)))
       (content (or (gethash "content" args) ""))
       (ws (structural-editing-mcp.workspace:get-workspace ws-id)))
    (unless filepath (error "filepath is required for create_file"))
    (structural-editing-mcp.workspace:add-file-to-workspace
      filepath
      :content
      content
      :dialect
      (or (parse-dialect-arg (gethash "dialect" args))
          (structural-editing-mcp.workspace:file-dialect filepath))
      :ctx
      ws)
    (fmt
      "File ~S created successfully in workspace ~S (staged in memory)."
      filepath
      ws-id)))

(defun manage-rebase-workspace (args ws-id)
  "Rebase WS-ID onto upstream workspace."
  (let*
      ((onto-id
         (or (gethash "source_id" args)
             (gethash "target_id" args)
             (gethash "target_workspace_id" args)
             (gethash "onto" args)
             (gethash "onto_workspace_id" args)
             "default"))
       (strat-str (or (gethash "strategy" args) "three-way"))
       (strategy (intern (string-upcase strat-str) :keyword))
       (res
         (structural-editing-mcp.workspace:rebase-workspace
           ws-id
           :onto-id
           onto-id
           :strategy
           strategy)))
    (fmt
      "Workspace ~S successfully rebased onto ~S (~A file(s) updated)."
      ws-id
      onto-id
      (length (getf res :updated-files)))))

(defun manage-reload-workspace
       (ws-id files force)
  "Reload workspace files from disk."
  (let*
      ((ws (structural-editing-mcp.workspace:get-workspace ws-id))
       (reloaded
         (structural-editing-mcp.workspace:reload-workspace
           :ctx
           ws
           :files
           files
           :force
           force)))
    (fmt "Workspace ~S reloaded ~A file(s) from disk." ws-id (length reloaded))))

;;; Execution & Tool Dispatch

(defun dispatch-tool-call
       (name path args dialect &optional (agent-id "default"))
  "Dispatch tool invocation by tool NAME to registered handler."
  (let
      ((handler
         (or (gethash name *mcp-tool-handlers*)
             (when (equal name "workspace_add_file")
               (gethash "workspace_create_file" *mcp-tool-handlers*)))))
    (if handler
      (funcall handler args :path path :dialect dialect :agent-id agent-id)
      (error "Tool not found: ~A" name))))

(defun send-tool-error-response
       (id message error-code error-type &optional extra-fields)
  "Send a standardized JSON-RPC error response for tool execution."
  (let
      ((payload
         (dict "content"
               (list (dict "type" "text" "text" message))
               "isError"
               t
               "errorCode"
               error-code
               "errorType"
               error-type)))
    (when extra-fields
      (loop for
            (k v) on extra-fields by #'cddr do
            (setf (gethash k payload) v)))
    (send-result id payload)))

(defun execute-mutation-tool-locked
       (name path args dialect agent-id)
  "Execute a mutation tool holding *WORKSPACE-LOCK* with OCC validation and rollback on error."
  (bt:with-lock-held
    (structural-editing-mcp.workspace:*workspace-lock*)
    (let
        ((target-path
           (if (equal name "ast_relocate")
             (to-list (href args "target_path"))
             path)))
      (structural-editing-mcp.workspace:validate-agent-edit
        :agent-id
        agent-id
        :target-path
        target-path)
      (let
          ((old-rev structural-editing-mcp.workspace:*workspace-revision*)
           (old-tree structural-editing-mcp.workspace:*workspace-tree*)
           (old-agent-rev
             (gethash agent-id structural-editing-mcp.workspace:*agent-views*)))
        (structural-editing-mcp.workspace:commit-agent-edit agent-id)
        (handler-case
            (dispatch-tool-call name path args dialect agent-id)
          (error (e)
            (setf structural-editing-mcp.workspace:*workspace-tree* old-tree)
            (setf structural-editing-mcp.workspace:*workspace-revision* old-rev)
            (if old-agent-rev
              (setf
                (gethash agent-id structural-editing-mcp.workspace:*agent-views*)
                old-agent-rev)
              (remhash agent-id structural-editing-mcp.workspace:*agent-views*))
            (error e)))))))

(defun execute-tool-call
       (name path args dialect agent-id)
  "Route tool execution based on whether it requires mutation locking or plain dispatch."
  (cond
    ((mutation-tool-p name)
      (execute-mutation-tool-locked name path args dialect agent-id))
    ((equal name "commit_workspace")
      (bt:with-lock-held
        (structural-editing-mcp.workspace:*workspace-lock*)
        (dispatch-tool-call name path args dialect agent-id)))
    (t (dispatch-tool-call name path args dialect agent-id))))

(defun handle-tools-call (id params)
  (let*
      ((name (href params "name"))
       (args (href params "arguments"))
       (path (to-list (href args "path")))
       (dialect (parse-dialect-arg (href args "dialect")))
       (agent-id (or (href args "agent_id") (href args "agent") "default"))
       (ws-id (or (href args "workspace_id") (href args "workspace") "default")))
    (handler-case
        (let*
            ((ws (structural-editing-mcp.workspace:get-workspace ws-id))
             (content
               (structural-editing-mcp.workspace:with-workspace-context
                 (ws)
                 (execute-tool-call name path args dialect agent-id))))
          (send-result id (dict "content" (list (dict "type" "text" "text" content)))))
      (structural-editing-mcp.conditions:occ-conflict-error
          (c)
        (send-tool-error-response id
                                  (format nil "~A" c)
                                  structural-editing-mcp.conditions:+error-code-occ-conflict+
                                  "occ_conflict"))
      (structural-editing-mcp.conditions:invalid-path-error
          (c)
        (send-tool-error-response id
                                  (format nil "Invalid path error: ~A" c)
                                  structural-editing-mcp.conditions:+error-code-invalid-params+
                                  "invalid_path"
                                  (list "path" (structural-editing-mcp.conditions:invalid-path-error-path c))))
      (structural-editing-mcp.conditions:sexp-parse-error
          (c)
        (send-tool-error-response id
                                  (format nil "Parse error: ~A" c)
                                  structural-editing-mcp.conditions:+error-code-parse-error+
                                  "parse_error"))
      (structural-editing-mcp.conditions:workspace-error
          (c)
        (send-tool-error-response id
                                  (format nil "Workspace error: ~A" c)
                                  structural-editing-mcp.conditions:+error-code-workspace-error+
                                  "workspace_error"))
      (error (e)
        (send-tool-error-response id
                                  (fmt "Error: ~A" e)
                                  structural-editing-mcp.conditions:+error-code-internal-error+
                                  "internal_error")))))