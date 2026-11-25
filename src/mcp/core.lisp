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
       (tree target-path old-name new-name &optional dialect)
  "Recursively rename all identifier leaf nodes matching OLD-NAME to NEW-NAME under TARGET-PATH."
  (structural-editing-mcp.refactor:rename-identifier
    tree
    old-name
    new-name
    :target-path target-path
    :dialect dialect))

;;; Tool Schema Builders & Definitions

(defun prop-path
       (&optional
        (desc
          "AST target selector: a hashnode handle string (e.g. \"#n402:3a9f\"), a 0-indexed integer array (e.g. [0, 0, 2]), or a symbolic path array (e.g. [\"main.lisp\", \"defun:foo\", 2])."))
  (dict "oneOf"
        (list (dict "type" "string") (dict "type" "array"))
        "description"
        desc))

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

(defun make-tool-annotations
       (&key title
        (read-only nil ro-p)
        (destructive nil dest-p)
        (idempotent nil idem-p)
        (open-world nil ow-p))
  "Build an MCP ToolAnnotations dict using yason:false for false booleans."
  (let ((d (make-hash-table :test #'equal)))
    (when title (setf (gethash "title" d) title))
    (when ro-p
      (setf (gethash "readOnlyHint" d)
            (if read-only
              t
              yason:false)))
    (when dest-p
      (setf (gethash "destructiveHint" d)
            (if destructive
              t
              yason:false)))
    (when idem-p
      (setf (gethash "idempotentHint" d)
            (if idempotent
              t
              yason:false)))
    (when ow-p
      (setf (gethash "openWorldHint" d)
            (if open-world
              t
              yason:false)))
    d))

(defun make-tool
       (name desc props &optional required title annotations output-schema)
  "Build an MCP tool definition hash table with optional title, annotations, and outputSchema."
  (let
      ((d
         (dict "name" name "description" desc "inputSchema" (tool-schema props required))))
    (when title (setf (gethash "title" d) title))
    (when annotations (setf (gethash "annotations" d) annotations))
    (when output-schema (setf (gethash "outputSchema" d) output-schema))
    d))

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
    "AST target selector: pass a hashnode handle string (e.g. \"#n402:3a9f\") from a previous preview for stale-write-safe addressing, a 0-indexed integer array (e.g. [0, 0, 2], or [] for workspace root), or a symbolic path array (e.g. [\"main.lisp\", \"defun:foo\", 2])."))

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
          (name
           (&key description title mutation (workspace t) annotations output-schema)
           params
           &body
           body)
  "Define an MCP tool, registering its JSON schema in *MCP-TOOL-DEFINITIONS*
and its execution handler in *MCP-TOOL-HANDLERS*."
  (let*
      ((handler-fn
         (intern
           (string-upcase (format nil "DISPATCH-MCP-TOOL-~A" (substitute #\- #\_ name)))))
       (effective-params
         (let ((res (copy-list params)))
           (when mutation
             (unless
                 (member 'expected_revision res :key #'car)
               (setf res
                     (append res
                             '((expected_revision :type
                                :integer
                                :doc
                                "Optional expected workspace revision number for optimistic concurrency checking."))))))
           (when workspace
             (unless (member 'workspace_id res :key #'car)
               (setf res (append res '((workspace_id :type :workspace-id)))))
             (unless (member 'agent_id res :key #'car)
               (setf res (append res '((agent_id :type :agent-id))))))
           res))
       (props-var (gensym "PROPS"))
       (reqs-var (gensym "REQS")))
    `(progn
       (let
           ((,props-var (make-hash-table :test #'equal)) (,reqs-var '()))
         ,@(loop for p in effective-params
                 for pname = (first p)
                 for key-str = (string-downcase (substitute #\_ #\- (string pname)))
                 for req-p = (getf (cdr p) :required)
                 collect `(setf (gethash ,key-str ,props-var) (build-prop-schema ',p))
                 when req-p
                 collect `(push ,key-str ,reqs-var))
         (register-tool-definition
           (make-tool ,name
                      ,description
                      ,props-var
                      (nreverse ,reqs-var)
                      ,title
                      ,annotations
                      ,output-schema)))
       (defun ,handler-fn
              (args &key (path nil) (dialect nil) (agent-id "default") &allow-other-keys)
         (declare (ignorable args path dialect agent-id))
         (let*
             ((raw-path (or path (to-list (gethash "path" args))))
              (path
                (if raw-path
                  (or
                    (structural-editing-mcp.tree:resolve-symbolic-path
                      structural-editing-mcp.workspace:*workspace-tree*
                      raw-path)
                    raw-path)
                  nil))
              (dialect (or dialect (parse-dialect-arg (gethash "dialect" args))))
              (agent-id
                (or (gethash "agent_id" args) (gethash "agent-id" args) agent-id "default"))
              ,@(loop for p in effective-params
                      for pname = (first p)
                      unless (member pname '(path dialect agent-id agent_id))
                      collect
                      (let
                          ((key-str (string-downcase (substitute #\_ #\- (string pname))))
                           (default (getf (cdr p) :default))
                           (has-default (not (eq (getf (cdr p) :default :missing) :missing))))
                        (if has-default
                          `(,pname
                             (multiple-value-bind (val present-p)
                                                  (gethash ,key-str args)
                               (if present-p
                                 val
                                 ,default)))
                          `(,pname (gethash ,key-str args))))))
           (declare (ignorable path dialect agent-id ,@(mapcar #'first effective-params)))
           ,@body))
       (setf (gethash ,name *mcp-tool-handlers*) #',handler-fn)
       ,@(when mutation
           `((unless
                 (member ,name *mutation-tools* :test #'string=)
               (setf *mutation-tools* (append *mutation-tools* (list ,name))))))
       ',handler-fn)))

(defun get-tools-list () *mcp-tool-definitions*)

(defun server-instructions-string ()
  "Return concise server workflow instructions for MCP discovery."
  "Use read_node with mode='skeleton' on initial reads, read_slice for deep nested nodes, and workspace_manage (fork/snapshot) before multi-step edits.")

(defun negotiate-protocol-version (requested)
  "Select a mutually supported MCP protocol version, defaulting to +DEFAULT-LEGACY-PROTOCOL-VERSION+."
  (cond
    ((and (stringp requested)
          (member requested +supported-protocol-versions+ :test #'string=))
      requested)
    (t +default-legacy-protocol-version+)))

(defun server-capabilities-dict ()
  "Build the MCP server capabilities object."
  (dict "tools"
        (dict "listChanged" yason:false)
        "resources"
        (dict "subscribe" t "listChanged" t)
        "extensions"
        (dict "io.modelcontextprotocol/skills" (dict "directoryRead" t))))

(defun maybe-wrap-mcp-2-0-result (res)
  "Attach resultType=\"complete\" when responding to a 2026-07-28 MCP 2.0 request."
  (when
      (and (mcp-2-0-request-p) (hash-table-p res) (null (gethash "resultType" res)))
    (setf (gethash "resultType" res) "complete"))
  res)

(defun handle-initialize (id params)
  "Handle legacy/2025 initialize handshake with version negotiation."
  (let*
      ((requested (and (hash-table-p params) (href params "protocolVersion")))
       (client-caps (and (hash-table-p params) (href params "capabilities")))
       (version (negotiate-protocol-version requested)))
    (setf *negotiated-protocol-version*
          version
          *negotiated-client-capabilities*
          client-caps
          *session-initialized-p*
          t)
    (send-result id
                 (dict "protocolVersion"
                       version
                       "capabilities"
                       (server-capabilities-dict)
                       "serverInfo"
                       (dict "name" "structural-editing-mcp" "version" +version+)
                       "instructions"
                       (server-instructions-string)))))

(defun handle-server-discover (id params)
  "Handle MCP 2.0 (2026-07-28) stateless server/discover request."
  (declare (ignore params))
  (send-result id
               (dict "resultType"
                     "complete"
                     "supportedVersions"
                     +supported-protocol-versions+
                     "capabilities"
                     (server-capabilities-dict)
                     "_meta"
                     (dict "io.modelcontextprotocol/serverInfo"
                           (dict "name" "structural-editing-mcp" "version" +version+))
                     "instructions"
                     (server-instructions-string))))

(defun handle-tools-list (id params)
  (declare (ignore params))
  (send-result id (maybe-wrap-mcp-2-0-result (dict "tools" (get-tools-list)))))

(defun parse-dialect-arg (dialect-str)
  "Parse an optional dialect string (e.g. \":clojure\" or \"common-lisp\") into a keyword."
  (when
      (and dialect-str (plusp (length dialect-str)))
    (intern (string-upcase (string-left-trim ":" dialect-str)) :keyword)))

;;; Tool Analysis & Workspace Execution Helpers

(defun run-tool-lint (tree path dialect args)
  "Run AST linter and return (values formatted-report structured-dict)."
  (let*
      ((rules (to-list (gethash "rules" args)))
       (findings
         (structural-editing-mcp.analysis:lint-ast tree
                                                   :path
                                                   path
                                                   :dialect
                                                   dialect
                                                   :rules
                                                   rules))
       (items
         (coerce
           (loop for f in findings
                 collect
                 (dict "rule"
                       (string-downcase (string (structural-editing-mcp.analysis:lint-finding-rule f)))
                       "path"
                       (coerce (or (structural-editing-mcp.analysis:lint-finding-path f) '()) 'vector)
                       "severity"
                       (string-downcase
                         (string (structural-editing-mcp.analysis:lint-finding-severity f)))
                       "message"
                       (structural-editing-mcp.analysis:lint-finding-message f)))
           'vector)))
    (values
      (structural-editing-mcp.analysis:format-lint-findings findings)
      (dict "count" (length findings) "findings" items))))

(defun run-tool-complexity
       (tree path dialect args)
  "Run cyclomatic complexity analysis and return (values formatted-report structured-dict)."
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
           min-d))
       (items
         (coerce
           (loop for r in results
                 collect
                 (dict "name"
                       (or (structural-editing-mcp.analysis:complexity-metrics-name r) "")
                       "kind"
                       (string-downcase
                         (string
                           (or (structural-editing-mcp.analysis:complexity-metrics-kind r) :unknown)))
                       "path"
                       (coerce
                         (or (structural-editing-mcp.analysis:complexity-metrics-path r) '())
                         'vector)
                       "cyclomaticComplexity"
                       (structural-editing-mcp.analysis:complexity-metrics-cyclomatic-complexity r)
                       "maxNestingDepth"
                       (structural-editing-mcp.analysis:complexity-metrics-max-nesting-depth r)
                       "formCount"
                       (structural-editing-mcp.analysis:complexity-metrics-form-count r)))
           'vector)))
    (values
      (structural-editing-mcp.analysis:format-complexity-report results)
      (dict "count" (length results) "metrics" items))))

(defun run-tool-duplicates (tree path args)
  "Run duplicate subtree detector and return (values formatted-report structured-dict)."
  (let*
      ((min-nodes (or (gethash "min_nodes" args) (gethash "min_size" args) 4))
       (min-depth (or (gethash "min_depth" args) 2))
       (min-occurrences (or (gethash "min_occurrences" args) 2))
       (exact
         (multiple-value-bind (val present-p)
                              (gethash "exact" args)
           (if present-p
             val
             t)))
       (results
         (structural-editing-mcp.analysis:find-duplicate-subtrees
           tree
           :path
           path
           :min-nodes
           min-nodes
           :min-depth
           min-depth
           :min-occurrences
           min-occurrences
           :exact
           exact))
       (items
         (coerce
           (loop for g in results
                 collect
                 (dict "occurrenceCount"
                       (structural-editing-mcp.analysis:duplicate-group-occurrence-count g)
                       "nodeCount"
                       (structural-editing-mcp.analysis:duplicate-group-node-count g)
                       "depth"
                       (structural-editing-mcp.analysis:duplicate-group-depth g)
                       "paths"
                       (coerce
                         (mapcar
                           (lambda (p)
                             (coerce (or p '()) 'vector))
                           (structural-editing-mcp.analysis:duplicate-group-paths g))
                         'vector)
                       "codeSnippet"
                       (structural-editing-mcp.analysis:duplicate-group-code-snippet g)))
           'vector)))
    (values
      (structural-editing-mcp.analysis:format-duplicate-report results)
      (dict "count" (length results) "groups" items))))

(defun run-tool-bindings (tree path dialect args)
  "Run variable binding analysis and return (values formatted-report structured-dict)."
  (let*
      ((inc-unused
         (multiple-value-bind (val present-p)
                              (gethash "include_unused" args)
           (if present-p
             val
             t)))
       (inc-shadowed
         (multiple-value-bind (val present-p)
                              (gethash "include_shadowed" args)
           (if present-p
             val
             t)))
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
           dialect))
       (items
         (coerce
           (loop for f in findings
                 collect
                 (dict "kind"
                       (string-downcase
                         (string (structural-editing-mcp.analysis:binding-finding-kind f)))
                       "variableName"
                       (structural-editing-mcp.analysis:binding-finding-variable-name f)
                       "path"
                       (coerce
                         (or (structural-editing-mcp.analysis:binding-finding-path f) '())
                         'vector)
                       "scopeKind"
                       (string-downcase
                         (string
                           (or (structural-editing-mcp.analysis:binding-finding-scope-kind f) :unknown)))
                       "message"
                       (structural-editing-mcp.analysis:binding-finding-message f)))
           'vector)))
    (values
      (structural-editing-mcp.analysis:format-binding-report findings)
      (dict "count" (length findings) "findings" items))))

(defun run-tool-suggestions
       (tree path dialect args)
  "Aggregate analysis findings and return (values formatted-report structured-dict)."
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
           dialect))
       (items
         (coerce
           (loop for s in suggestions
                 collect
                 (dict "category"
                       (string-downcase
                         (string (structural-editing-mcp.analysis:refactoring-suggestion-category s)))
                       "priority"
                       (string-downcase
                         (string (structural-editing-mcp.analysis:refactoring-suggestion-priority s)))
                       "path"
                       (coerce
                         (or (structural-editing-mcp.analysis:refactoring-suggestion-path s) '())
                         'vector)
                       "recommendedTool"
                       (or
                         (structural-editing-mcp.analysis:refactoring-suggestion-recommended-tool s)
                         "")
                       "description"
                       (structural-editing-mcp.analysis:refactoring-suggestion-description s)))
           'vector)))
    (values
      (structural-editing-mcp.analysis:format-refactoring-suggestions suggestions)
      (dict "count" (length suggestions) "suggestions" items))))

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

(defun node-preview-structured-dict (node)
  "Build structured metadata dict for an AST node preview."
  (if (null node)
    (dict "found" yason:false)
    (let
        ((path (structural-editing-mcp.tree:get-node-path node))
         (handle (structural-editing-mcp.tree:get-node-id-handle node))
         (tag (structural-editing-mcp.tree:get-node-tag node))
         (children (structural-editing-mcp.tree:get-node-children node)))
      (dict "found"
            t
            "revision"
            structural-editing-mcp.workspace:*workspace-revision*
            "path"
            (coerce (or path '()) 'vector)
            "handle"
            (or handle "")
            "tag"
            (string-downcase (string tag))
            "childCount"
            (length children)))))

(defun workspace-status-structured-dict (st)
  "Convert workspace-status plist ST to a JSON-serializable dict."
  (dict "id"
        (getf st :id)
        "parentId"
        (or (getf st :parent-id) "")
        "revision"
        (getf st :revision)
        "baseRevision"
        (getf st :base-revision)
        "dirtyFiles"
        (coerce (or (getf st :dirty-files) '()) 'vector)
        "cleanFiles"
        (coerce (or (getf st :clean-files) '()) 'vector)
        "snapshots"
        (coerce (or (getf st :snapshots) '()) 'vector)))

(defun workspace-diff-structured-dict (df)
  "Convert diff-workspaces plist DF to a JSON-serializable dict."
  (dict "sourceId"
        (getf df :source-id)
        "targetId"
        (getf df :target-id)
        "sourceOnly"
        (coerce (or (getf df :source-only) '()) 'vector)
        "targetOnly"
        (coerce (or (getf df :target-only) '()) 'vector)
        "sourceModifiedOnly"
        (coerce (or (getf df :source-modified-only) '()) 'vector)
        "targetModifiedOnly"
        (coerce (or (getf df :target-modified-only) '()) 'vector)
        "astMergeable"
        (coerce (or (getf df :ast-mergeable) '()) 'vector)
        "modifiedInBoth"
        (coerce (or (getf df :modified-in-both) '()) 'vector)))

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
    (when (mcp-2-0-request-p) (setf (gethash "resultType" payload) "complete"))
    (when extra-fields
      (loop for (k v) on extra-fields by #'cddr do (setf (gethash k payload) v)))
    (send-result id payload)))

(defun execute-mutation-tool-locked
       (name path args dialect agent-id)
  "Execute a mutation tool holding *WORKSPACE-LOCK* with OCC validation and rollback on error."
  (bt:with-lock-held
    (structural-editing-mcp.workspace:*workspace-lock*)
    (let*
        ((raw-target
           (if (equal name "ast_relocate")
             (or (to-list (href args "target_path"))
                 (to-list (href args "source_path"))
                 path)
             path))
         (target-path
           (when raw-target
             (or
               (structural-editing-mcp.tree:resolve-symbolic-path
                 structural-editing-mcp.workspace:*workspace-tree*
                 raw-target)
               (if
                   (and (listp raw-target) (every #'integerp raw-target))
                 raw-target
                 (error
                   'structural-editing-mcp.conditions:invalid-path-error
                   :path
                   raw-target
                   :tree
                   structural-editing-mcp.workspace:*workspace-tree*
                   :message
                   (fmt
                     "Could not resolve handle or symbolic path ~S (node may have been deleted or replaced)."
                     raw-target))))))
         (expected-rev (href args "expected_revision")))
      (when
          (and (integerp expected-rev)
               (/= expected-rev structural-editing-mcp.workspace:*workspace-revision*))
        (error
          'structural-editing-mcp.conditions:occ-conflict-error
          :agent-id
          agent-id
          :expected-revision
          expected-rev
          :current-revision
          structural-editing-mcp.workspace:*workspace-revision*
          :suggested-path
          (or (butlast target-path) target-path)))
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
            (dispatch-tool-call name target-path args dialect agent-id)
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
    ((member name
             '
             ("commit_workspace" "ast_search"
              "ast_lint"
              "ast_complexity_metrics"
              "ast_find_duplicates"
              "ast_analyze_bindings"
              "ast_suggest_refactorings")
             :test
             #'string=)
      (bt:with-lock-held
        (structural-editing-mcp.workspace:*workspace-lock*)
        (dispatch-tool-call name path args dialect agent-id)))
    (t (dispatch-tool-call name path args dialect agent-id))))

(defun handle-tools-call (id params)
  (let*
      ((name (href params "name"))
       (args (or (href params "arguments") (make-hash-table :test #'equal)))
       (path (to-list (href args "path")))
       (dialect (parse-dialect-arg (href args "dialect")))
       (agent-id (or (href args "agent_id") (href args "agent") "default"))
       (ws-id (or (href args "workspace_id") (href args "workspace") "default")))
    (handler-case
        (let*
            ((ws (structural-editing-mcp.workspace:get-workspace ws-id)))
          (multiple-value-bind (content structured)
                               (structural-editing-mcp.workspace:with-workspace-context
                                 (ws)
                                 (execute-tool-call name path args dialect agent-id))
            (let
                ((res (dict "content" (list (dict "type" "text" "text" content)))))
              (when structured (setf (gethash "structuredContent" res) structured))
              (when (mcp-2-0-request-p) (setf (gethash "resultType" res) "complete"))
              (send-result id res)
              (when
                  (and
                    (or (mutation-tool-p name)
                        (member name
                                '("workspace_create_file" "workspace_merge"
                                  "workspace_rebase"
                                  "workspace_manage")
                                :test
                                #'string=))
                    (fboundp 'notify-workspace-resources-changed))
                (funcall 'notify-workspace-resources-changed
                         ws-id
                         :list-changed-p
                         (not
                           (null
                             (member name '("workspace_create_file" "workspace_manage") :test #'string=))))))))
      (structural-editing-mcp.conditions:occ-conflict-error
          (c)
        (send-tool-error-response id
                                  (format nil "~A" c)
                                  structural-editing-mcp.conditions:+error-code-occ-conflict+
                                  "occ_conflict"))
      (structural-editing-mcp.conditions:hash-mismatch-error
          (c)
        (let*
            ((tpath (structural-editing-mcp.conditions:hash-mismatch-target-path c))
             (preview
               (ignore-errors
                 (let
                     ((ws (structural-editing-mcp.workspace:get-workspace ws-id)))
                   (structural-editing-mcp.workspace:with-workspace-context
                     (ws)
                     (format-node-slice
                       structural-editing-mcp.workspace:*workspace-tree*
                       (or (butlast tpath) tpath)
                       (dict "depth" 2 "mode" "skeleton")))))))
          (send-tool-error-response id
                                    (format nil "~A~@[~%~%~A~]" c preview)
                                    structural-editing-mcp.conditions:+error-code-occ-conflict+
                                    "hash_mismatch"
                                    (list "handle"
                                          (structural-editing-mcp.conditions:hash-mismatch-handle c)
                                          "expected_hash"
                                          (structural-editing-mcp.conditions:hash-mismatch-expected c)
                                          "actual_hash"
                                          (structural-editing-mcp.conditions:hash-mismatch-actual c)
                                          "target_path"
                                          tpath))))
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