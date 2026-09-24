(defpackage :structural-editing-mcp.mcp
  (:use :cl
        :alexandria
        :trivia
        :structural-editing-mcp.version
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.edit
        :structural-editing-mcp.analysis
        :structural-editing-mcp.conditions
        :structural-editing-mcp.workspace)
  (:import-from :serapeum :dict :trim-whitespace :ellipsize :fmt :href :defconst)
  (:export :start-server :handle-message :dict :start-worker-pool :stop-worker-pool))

(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

;;; JSON-RPC & MCP Utilities

(defvar *stdout-lock* (bt:make-lock "stdout-lock")
  "Mutex serializing output to *standard-output* across concurrent worker threads.")

(defun to-list (val)
  "Ensure val is a list, converting from vector if necessary."
  (if (vectorp val) (coerce val 'list) val))

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

(defvar *worker-queue* nil)
(defvar *worker-queue-lock* (bt:make-lock "worker-queue-lock"))
(defvar *worker-queue-cvar* (bt:make-condition-variable :name "worker-queue-cvar"))
(defvar *worker-threads* nil)
(defvar *worker-pool-running* nil)
(defparameter *default-worker-count* 4)

(defun worker-loop ()
  (loop
    (let ((task nil))
      (bt:with-lock-held (*worker-queue-lock*)
        (loop while (and *worker-pool-running* (null *worker-queue*))
              do (bt:condition-wait *worker-queue-cvar* *worker-queue-lock*))
        (when (null *worker-queue*)
          (unless *worker-pool-running*
            (return)))
        (setf task (pop *worker-queue*)))
      (when task
        (handler-case
            (funcall task)
          (error (e)
            (format *error-output* "~&[Worker Error] ~A~%" e)
            (force-output *error-output*)))))))

(defun start-worker-pool (&optional (num-workers *default-worker-count*))
  "Initialize and start the worker thread pool for parallel JSON-RPC request processing."
  (setf *worker-queue* nil)
  (setf *worker-pool-running* t)
  (setf *worker-threads*
        (loop repeat num-workers
              collect (bt:make-thread
                       (lambda () (worker-loop))
                       :name "mcp-worker-thread"))))

(defun stop-worker-pool ()
  "Stop all worker threads in the worker pool."
  (setf *worker-pool-running* nil)
  (bt:with-lock-held (*worker-queue-lock*)
    (loop repeat (* 2 (max 1 (length *worker-threads*)))
          do (bt:condition-notify *worker-queue-cvar*)))
  (dolist (th *worker-threads*)
    (ignore-errors (bt:join-thread th)))
  (setf *worker-threads* nil))

(defun enqueue-task (task-thunk)
  "Enqueue TASK-THUNK for processing by the worker pool."
  (bt:with-lock-held (*worker-queue-lock*)
    (setf *worker-queue* (append *worker-queue* (list task-thunk)))
    (bt:condition-notify *worker-queue-cvar*)))

;;; Node Presentation & Discovery Helpers


(defun truncate-preview-string (raw-str &key (max-length 70))
  "Format RAW-STR into a single line trimmed of newlines, capped at MAX-LENGTH with ellipsis."
  (ellipsize (substitute #\space #\newline (trim-whitespace (or raw-str ""))) max-length))

(defun print-children-tree (s node current-depth max-depth base-path)
  (when (and (< current-depth max-depth)
             (not (member (structural-editing-mcp.tree:get-node-tag node) '(:leaf :comment))))
    (let ((children (structural-editing-mcp.tree:get-node-children node)))
      (when children
        (loop for child in children
              for idx from 0
              for cpath = (or (structural-editing-mcp.tree:get-node-path child)
                              (append base-path (list idx)))
              for ctag = (structural-editing-mcp.tree:get-node-tag child)
              for snippet = (truncate-preview-string
                              (structural-editing-mcp.parser:sexp-to-string child))
              for indent = (make-string (* (1+ current-depth) 2) :initial-element #\space)
              do
              (format s "~A[~{~A~^, ~}] ~A: ~A~%" indent cpath ctag snippet)
              (print-children-tree s child (1+ current-depth) max-depth cpath))))))

(defun format-children-preview (s children path depth &optional (label-suffix ""))
  "Format child nodes with paths, tags, and preview snippets up to DEPTH."
  (when children
    (format s "~%Children (~A~A):~%" (length children) label-suffix)
    (loop for child in children
          for idx from 0
          for cpath = (or (structural-editing-mcp.tree:get-node-path child)
                          (append path (list idx)))
          for ctag = (structural-editing-mcp.tree:get-node-tag child)
          for snippet = (truncate-preview-string
                          (structural-editing-mcp.parser:sexp-to-string child))
          do
          (format s "  [~{~A~^, ~}] ~A: ~A~%" cpath ctag snippet)
          (when (> depth 1)
            (print-children-tree s child 1 depth cpath)))))

(defun format-dialect-node-preview (s dialect-node d-path)
  "Format preview for a single DIALECT-NODE under D-PATH to stream S."
  (let* ((d-tag (structural-editing-mcp.tree:get-node-tag dialect-node))
         (file-nodes (structural-editing-mcp.tree:get-node-children dialect-node)))
    (format s "  [~{~A~^, ~}] ~A (~A file~:P):~%"
            d-path d-tag (length file-nodes))
    (loop for file-node in file-nodes
          for f-idx from 0
          for f-path = (or (structural-editing-mcp.tree:get-node-path file-node)
                           (append d-path (list f-idx)))
          for filepath = (structural-editing-mcp.workspace:get-filepath f-path)
          for form-count = (length (structural-editing-mcp.tree:get-node-children file-node))
          do
          (format s "    [~{~A~^, ~}] :FILE (~A) — ~A top-level forms~%"
                  f-path (or filepath "unknown") form-count))))

(defun format-workspace-preview (s children)
  "Format preview for workspace root to stream S."
  (if (null children)
    (format s "Workspace is empty. Provide load_files in read_node to load files into the workspace.~%")
    (progn
      (format s "Active Dialects (~A):~%" (length children))
      (loop for dialect-node in children
            for d-idx from 0
            for d-path = (or (structural-editing-mcp.tree:get-node-path dialect-node) (list d-idx))
            do (format-dialect-node-preview s dialect-node d-path)))))

(defun format-dialect-preview (s children path tag)
  "Format preview for a dialect partition node to stream S."
  (format s "Dialect: ~A~%" tag)
  (if (null children)
    (format s "No files loaded for this dialect.~%")
    (progn
      (format s "Files Loaded (~A):~%" (length children))
      (loop for file-node in children
            for idx from 0
            for f-path = (or (structural-editing-mcp.tree:get-node-path file-node)
                             (append path (list idx)))
            for filepath = (structural-editing-mcp.workspace:get-filepath f-path)
            for form-count = (length (structural-editing-mcp.tree:get-node-children file-node))
            do
            (format s "  [~{~A~^, ~}] :FILE (~A) — ~A top-level forms~%"
                    f-path (or filepath "unknown") form-count)))))

(defun format-node-preview (node &key (depth 2))
  "Format a node with its path, tag, rendered code, and summary of children up to DEPTH."
  (if (null node)
    "Node not found at given path."
    (let* ((path (structural-editing-mcp.tree:get-node-path node))
           (tag (structural-editing-mcp.tree:get-node-tag node))
           (children (structural-editing-mcp.tree:get-node-children node))
           (code (structural-editing-mcp.parser:sexp-to-string node)))
      (with-output-to-string (s)
        (format s "Workspace Revision: ~D~%" structural-editing-mcp.workspace:*workspace-revision*)
        (format s "Path: ~A~%" (or path "()"))
        (format s "Tag: ~A~%" tag)
        (cond
          ((eq tag :workspace)
            (format-workspace-preview s children))
          ((member tag structural-editing-mcp.workspace:*known-dialects*)
            (format-dialect-preview s children path tag))
          ((eq tag :file)
            (let ((filepath (structural-editing-mcp.workspace:get-filepath path)))
              (when filepath (format s "File: ~A~%" filepath)))
            (format s "Code:~%~A~%" code)
            (format-children-preview s children path depth " top-level forms"))
          (t
            (format s "Code:~%~A~%" code)
            (format-children-preview s children path depth)))))))

;;; AST Mutation Dispatchers


(defparameter *delimiter-name-map*
  (dict "paren" :paren "()" :paren "" :paren
        "square" :square "bracket" :square "[]" :square
        "curly" :curly "brace" :curly "{}" :curly)
  "Map of wrapper aliases to AST delimiter keywords.")

(defun parse-delimiter-type (wrapper-str)
  "Map WRAPPER-STR to :paren, :square, :curly, or NIL if it represents a custom wrapper form."
  (let* ((clean-str (string-trim '(#\Space #\Tab #\Newline #\:) (or wrapper-str "")))
         (lower (string-downcase clean-str)))
    (gethash lower *delimiter-name-map*)))

(defun wrap-node-with-custom-form (tree path wrapper-str)
  "Wrap node at PATH using the custom form expression in WRAPPER-STR."
  (let* ((parsed (structural-editing-mcp.parser:string-to-sexp wrapper-str))
         (expr (first (structural-editing-mcp.tree:get-node-children parsed))))
    (if (and expr (structural-editing-mcp.tree:get-node-children expr))
      (let ((target-node (structural-editing-mcp.tree:get-node-at-path tree path)))
        (structural-editing-mcp.tree:update-node-at-path
          tree
          path
          (lambda (node)
            (declare (ignore node))
            (match expr ((node p tag children) `(:path ,p ,tag ,@children ,target-node))))))
      (structural-editing-mcp.edit:wrap-node tree path :paren))))

(defun wrap-range-with-custom-form (tree parent-path start-idx end-index wrapper-str)
  "Wrap range of nodes under PARENT-PATH using custom form expression in WRAPPER-STR."
  (let* ((parsed (structural-editing-mcp.parser:string-to-sexp wrapper-str))
         (expr (first (structural-editing-mcp.tree:get-node-children parsed))))
    (if (and expr (structural-editing-mcp.tree:get-node-children expr))
      (structural-editing-mcp.tree:update-node-at-path
        tree
        parent-path
        (lambda (parent)
          (match parent
                 ((node p ptag children)
                  (let ((before (subseq children 0 start-idx))
                        (slice (subseq children start-idx (1+ end-index)))
                        (after (subseq children (1+ end-index))))
                    (match expr
                           ((node _ tag expr-children)
                            `(:path ,p ,ptag ,@before (:path ,p ,tag ,@expr-children ,@slice) ,@after)))))
                 (_ parent))))
      (structural-editing-mcp.edit:wrap-range tree parent-path start-idx end-index :paren))))

(defun perform-wrap-single (tree path wrapper-str delim)
  "Wrap a single node at PATH with DELIM or custom WRAPPER-STR."
  (if delim
    (structural-editing-mcp.edit:wrap-node tree path delim)
    (wrap-node-with-custom-form tree path wrapper-str)))

(defun perform-wrap-range (tree parent-path start-idx end-index wrapper-str delim)
  "Wrap a range of nodes from START-IDX to END-INDEX under PARENT-PATH."
  (if delim
    (structural-editing-mcp.edit:wrap-range tree parent-path start-idx end-index delim)
    (wrap-range-with-custom-form tree parent-path start-idx end-index wrapper-str)))

(defun resolve-wrap-parent-and-start (path index)
  "Resolve parent path and starting index for a range wrap."
  (let ((parent-path (cond (index path)
                           ((null (cdr path)) path)
                           (t (butlast path))))
        (start-idx (cond (index index)
                         ((null (cdr path)) 0)
                         (t (lastcar path)))))
    (values parent-path start-idx)))

(defun perform-wrap (tree path wrapper-str &optional end-index index)
  "Wrap the node at PATH, or range of nodes from START-INDEX to END-INDEX under PARENT-PATH."
  (let ((delim (parse-delimiter-type wrapper-str)))
    (if (null end-index)
      (perform-wrap-single tree path wrapper-str delim)
      (multiple-value-bind (parent-path start-idx) (resolve-wrap-parent-and-start path index)
        (perform-wrap-range tree parent-path start-idx end-index wrapper-str delim)))))

(defun resolve-parent-and-index (tree target-path &optional index)
  "Resolve target parent path and index for move or copy."
  (if index
    (values target-path index)
    (if (null (cdr target-path))
      (values target-path
              (length (structural-editing-mcp.tree:get-node-children
                        (structural-editing-mcp.tree:get-node-at-path tree target-path))))
      (values (butlast target-path) (lastcar target-path)))))

(defun perform-insert (tree path new-node-str &optional index)
  "Insert NEW-NODE-STR into TREE. If INDEX is provided, PATH is the parent.
Otherwise, PATH specifies the target location (parent is (butlast path), index is (lastcar path))."
  (multiple-value-bind (parent-path target-idx) (resolve-parent-and-index tree path index)
    (structural-editing-mcp.edit:insert-expression
      tree
      parent-path
      target-idx
      new-node-str)))

(defun format-mutation-result (message target-path tree)
  (let*
      ((preview-path
         (cond
           ((null target-path) nil)
           ((null (cdr target-path)) target-path)
           (t (butlast target-path))))
       (preview-node
         (and
           preview-path
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

(defun perform-rename (tree target-path old-name new-name)
  "Recursively rename all leaf nodes matching OLD-NAME to NEW-NAME under TARGET-PATH."
  (let
      ((lower-old (string-downcase old-name))
       (parsed-new
         (first
           (structural-editing-mcp.tree:get-node-children
             (structural-editing-mcp.parser:string-to-sexp new-name)))))
    (labels
        ((walk
             (node)
           (match
             node
             ((leaf path val)
              (let*
                  ((str (structural-editing-mcp.parser::format-atom val))
                   (lower-str (string-downcase str)))
                (if
                    (string= lower-old lower-str)
                  ;; Replace with the new parsed leaf/node

                  (match
                    parsed-new
                    ((node _ tag children) ` (:path ,path ,tag ,@children))
                    ((leaf _ new-val) ` (:path ,path :leaf ,new-val))
                    (_ node))
                  node)))
             ((node path tag children) (list* :path path tag (mapcar #'walk children)))
             (_ node))))
      (if
          target-path
        (structural-editing-mcp.tree:update-node-at-path tree target-path #'walk)
        (walk tree)))))

;;; Tool Definitions


;;; Tool Schema Builders & Definitions

(defun prop-path (&optional (desc "0-indexed array of integers specifying the AST path."))
  (dict "type" "array"
        "items" (dict "type" "integer")
        "description" desc))

(defun prop-string (desc &key enum)
  (let ((d (dict "type" "string" "description" desc)))
    (when enum (setf (gethash "enum" d) enum))
    d))

(defun prop-integer (desc)
  (dict "type" "integer" "description" desc))

(defun prop-boolean (desc)
  (dict "type" "boolean" "description" desc))

(defun prop-string-array (desc)
  (dict "type" "array"
        "items" (dict "type" "string")
        "description" desc))

(defun tool-schema (props &optional required)
  (let ((s (dict "type" "object" "properties" props)))
    (when required (setf (gethash "required" s) required))
    s))

(defun make-tool (name desc props &optional required)
  (dict "name" name
        "description" desc
        "inputSchema" (tool-schema props required)))

(defparameter +prop-path+
  (prop-path "0-indexed array of integers specifying the AST path. Omit or pass [] for the workspace root, [0] for dialect 0 (e.g. :common-lisp), [0, 0] for file 0 in dialect 0, [0, 0, 2] for top-level form 2 in file 0, [0, 0, 3, 1] for child 1 of form 3."))

(defparameter +prop-load-files+
  (prop-string-array "Optional list of file or directory paths to load into the workspace (e.g. ['/path/to/project'] or ['/path/to/file.lisp']). Directories are recursively scanned for Lisp source files."))

(defparameter +prop-dialect+
  (prop-string "Optional Lisp dialect override (:common-lisp, :clojure, :scheme, :emacs-lisp, :fennel). Inferred if omitted."))

(defparameter +prop-agent-id+
  (prop-string "Optional identifier for the calling agent (e.g. 'agent-1', 'refactorer'). Used for automatic multi-agent concurrency tracking."))

(defun get-tools-list ()
  (list
    (make-tool
      "read_node"
      "Inspect any node in the AST or workspace. Returns rendered code and a nested tree of child paths up to 'depth' levels. BEST PRACTICE: Always read parent forms (e.g. [0, 10]) to see the entire expression and its child paths at once—do NOT probe child indices one-by-one. Use 'ast_search' to find symbols/calls across the workspace."
      (dict "path" +prop-path+
            "depth" (prop-integer "Recursion depth for displaying nested children and their paths (default: 2). Use depth 1 for only immediate children, 2 or 3 to inspect deeper sub-expressions.")
            "load_files" +prop-load-files+
            "agent_id" +prop-agent-id+))

    (make-tool
      "ast_modify"
      "Mutate AST nodes. Actions: 'insert' (adds new_node before path, or at child index if index is given), 'overwrite' (replaces node at path with new_node), 'wrap' (wraps node at path with parens, brackets, or an enclosing form). NOTE: Automatically returns an updated preview of the enclosing parent node; separate verification reads are unnecessary."
      (dict "path" (prop-path "Target AST path (e.g. [0, 2] to target form 2 in file 0).")
            "action" (prop-string "The modification action to perform." :enum (list "insert" "overwrite" "wrap"))
            "new_node" (prop-string "For insert/overwrite: the S-expression code string (e.g. '(defun foo () 42)'). For wrap: delimiter keyword (':paren', ':square', ':curly') or enclosing form string (e.g. '(when condition)').")
            "index" (prop-integer "Optional child index for insert. If omitted when inserting, uses the last element of path.")
            "end_index" (prop-integer "Optional ending child index for range wrapping with action 'wrap'.")
            "agent_id" +prop-agent-id+)
      (list "path" "action" "new_node"))

    (make-tool
      "ast_remove"
      "Remove or unwrap AST nodes. Actions: 'delete' (deletes the node at path), 'unwrap' (removes enclosing collection, spilling children into parent), 'promote' (replaces parent node with the node at path). NOTE: Automatically returns an updated preview."
      (dict "path" (prop-path "The AST path of the node to remove/unwrap/promote.")
            "action" (prop-string "Removal action to perform." :enum (list "delete" "unwrap" "promote"))
            "agent_id" +prop-agent-id+)
      (list "path" "action"))

    (make-tool
      "ast_relocate"
      "Move, copy, swap, merge, or split AST nodes. Actions: 'move' (moves source_path to target_path), 'copy' (duplicates source_path to target_path), 'swap' (swaps nodes at source_path and target_path), 'merge' (merges sibling collection nodes), 'split' (splits target collection node at index). NOTE: Automatically returns an updated preview."
      (dict "source_path" (prop-path "Path to source node (optional for split).")
            "target_path" (prop-path "Target path for move/copy/swap/merge/split (e.g. [0, 2] places before index 2 in file 0).")
            "action" (prop-string "Relocation action to perform." :enum (list "move" "copy" "swap" "merge" "split"))
            "index" (prop-integer "Child index within target collection to split at (action 'split') or destination insertion index (actions 'move', 'copy').")
            "agent_id" +prop-agent-id+)
      (list "target_path" "action"))

    (make-tool
      "ast_search"
      "Search the workspace or a subtree for symbols, identifiers, function calls, or literal values. Fast AST-aware token searching."
      (dict "query" (prop-string "Symbol or text to search for (case-insensitive substring/symbol match).")
            "path" (prop-path "Optional AST path to constrain the search scope. If omitted, searches the entire workspace.")
            "agent_id" +prop-agent-id+)
      (list "query"))

    (make-tool
      "ast_rename"
      "Rename all occurrences of an identifier/symbol across the workspace or within a specific subtree. Operates strictly on symbol leaf nodes, preserving comments and string literals."
      (dict "old_name" (prop-string "The exact symbol/string to replace (e.g. 'make-api-call').")
            "new_name" (prop-string "The new symbol/string to replace it with (e.g. 'execute-api-call').")
            "path" (prop-path "Optional AST path to constrain the bulk rename to a specific subtree. If omitted, renames globally across the workspace.")
            "agent_id" +prop-agent-id+)
      (list "old_name" "new_name"))

    (make-tool
      "ast_replace_pattern"
      "Search the workspace for a structural Lisp pattern and replace it with a new pattern, preserving matched variables (e.g. pattern='(foo ?x ?y)', replacement='(bar ?y ?x)')."
      (dict "pattern" (prop-string "The pattern to match. Variables start with '?' (e.g. '(make-api-call ?method ?url ?headers ?body)').")
            "replacement" (prop-string "The replacement template (e.g. '(make-api-call ?url ?method :headers ?headers :body ?body)').")
            "agent_id" +prop-agent-id+)
      (list "pattern" "replacement"))

    (make-tool
      "ast_extract_variable"
      "Extracts an AST node into a local `let` binding wrapped around its immediate parent."
      (dict "path" (prop-path "AST path of the node to extract.")
            "variable_name" (prop-string "The name of the new variable to bind it to.")
            "agent_id" +prop-agent-id+)
      (list "path" "variable_name"))

    (make-tool
      "ast_extract_function"
      "Extracts an AST node into a new top-level function definition and replaces the original node with a call to the new function."
      (dict "path" (prop-path "AST path of the node to extract into a function.")
            "function_name" (prop-string "The name of the new function.")
            "params" (prop-string-array "Optional list of parameter names for the new function.")
            "agent_id" +prop-agent-id+)
      (list "path" "function_name"))

    (make-tool
      "ast_lint"
      "Run static analysis and structural linting to identify code smells, anti-patterns, and opportunities for refactoring. Returns a list of findings with AST paths, messages, severity, and suggested quick-fixes."
      (dict "path" (prop-path "Optional AST path to lint a specific node or file. If omitted, lints the entire workspace.")
            "dialect" +prop-dialect+
            "rules" (prop-string-array "Optional list of rule IDs to filter by (e.g. ['if-progn-to-when', 'single-clause-cond']).")
            "agent_id" +prop-agent-id+))

    (make-tool
      "ast_complexity_metrics"
      "Calculate structural and cyclomatic complexity metrics for functions and top-level forms. Reports branch complexity, maximum nesting depth, AST node counts, and automated recommendations for code extraction."
      (dict "path" (prop-path "Optional AST path to evaluate a specific form, file, or subtree. If omitted, analyzes all forms across the workspace.")
            "min_complexity" (prop-integer "Optional minimum cyclomatic complexity threshold to filter results (default: 1).")
            "min_depth" (prop-integer "Optional minimum parenthetical nesting depth threshold to filter results (default: 1).")
            "dialect" +prop-dialect+
            "agent_id" +prop-agent-id+))

    (make-tool
      "ast_find_duplicates"
      "Find repeated expressions and structural code clones across the workspace or within a file. Groups duplicate subtrees, filters redundant child occurrences, and recommends extraction into helper functions or local variables."
      (dict "path" (prop-path "Optional AST path to constrain the duplicate search to a specific file or subtree. If omitted, searches the entire workspace.")
            "min_nodes" (prop-integer "Optional minimum AST node count threshold for subtrees (default: 4).")
            "min_depth" (prop-integer "Optional minimum parenthetical nesting depth threshold for subtrees (default: 2).")
            "exact" (prop-boolean "If true (default), matches exact identical code. If false, matches structural clones where variables/literals can vary.")
            "agent_id" +prop-agent-id+))

    (make-tool
      "ast_analyze_bindings"
      "Analyze lexical scope and variable bindings to detect unused variables and shadowed bindings across dialects. Recommends removals via ast_remove or renamings via ast_rename."
      (dict "path" (prop-path "Optional AST path to constrain analysis to a specific file or subtree. If omitted, analyzes all files in the workspace.")
            "include_unused" (prop-boolean "If true (default), reports variables defined but never used in their lexical scope.")
            "include_shadowed" (prop-boolean "If true (default), reports local variables that shadow outer bindings with the same name.")
            "dialect" +prop-dialect+
            "agent_id" +prop-agent-id+))

    (make-tool
      "ast_suggest_refactorings"
      "Multi-engine refactoring advisor. Aggregates and prioritizes findings from anti-pattern linting, structural complexity metrics, duplicate code clones, and variable binding analysis into an actionable refactoring plan."
      (dict "path" (prop-path "Optional AST path to evaluate a specific form, file, or subtree. If omitted, audits the entire workspace.")
            "min_priority" (prop-string "Optional minimum priority filter ('high', 'medium', 'low', defaults to 'low').")
            "categories" (prop-string-array "Optional list of categories to include ('lint', 'complexity', 'duplicate', 'binding').")
            "dialect" +prop-dialect+
            "agent_id" +prop-agent-id+))

    (make-tool
      "commit_workspace"
      "Persists all in-memory workspace modifications back to their respective files on disk."
      (dict "agent_id" +prop-agent-id+))))

;;; Handlers


(defun handle-initialize (id params)
  (declare (ignore params))
  (send-result
    id
    (dict
      "protocolVersion"
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
  (when (and dialect-str (plusp (length dialect-str)))
    (intern (string-upcase (string-left-trim ":" dialect-str)) :keyword)))

(defun handle-tool-read-node (path args &optional (agent-id "default"))
  "Handle the read_node tool execution."
  (bt:with-lock-held (structural-editing-mcp.workspace:*workspace-lock*)
    (let ((files-to-load (to-list (gethash "load_files" args))))
      (when files-to-load
        (structural-editing-mcp.workspace:load-into-workspace files-to-load)))
    (unless structural-editing-mcp.workspace:*workspace-tree*
      (structural-editing-mcp.workspace:init-workspace))
    (structural-editing-mcp.workspace:record-agent-read agent-id)
    (let* ((depth (or (gethash "depth" args) 2))
           (node (structural-editing-mcp.tree:resolve-tree-scope
                   structural-editing-mcp.workspace:*workspace-tree*
                   path)))
      (format-node-preview node :depth depth))))

(defun handle-tool-ast-modify (path args)
  "Handle the ast_modify tool execution."
  (let* ((action (gethash "action" args))
         (new-node-str (gethash "new_node" args))
         (index (gethash "index" args))
         (end-index (gethash "end_index" args))
         (tree structural-editing-mcp.workspace:*workspace-tree*))
    (setf structural-editing-mcp.workspace:*workspace-tree*
          (cond
            ((equal action "insert")
              (perform-insert tree path new-node-str index))
            ((equal action "overwrite")
              (structural-editing-mcp.edit:overwrite-expression tree path new-node-str))
            ((equal action "wrap")
              (perform-wrap tree path new-node-str end-index index))
            (t (error "Unknown action: ~A" action))))
    (format-mutation-result
      (fmt "Successfully executed ~A at ~A" action path)
      path
      structural-editing-mcp.workspace:*workspace-tree*)))

(defun handle-tool-ast-remove (path args)
  "Handle the ast_remove tool execution."
  (let* ((action (gethash "action" args))
         (tree structural-editing-mcp.workspace:*workspace-tree*))
    (setf structural-editing-mcp.workspace:*workspace-tree*
          (cond
            ((equal action "delete")
              (structural-editing-mcp.edit:delete-node tree path))
            ((equal action "unwrap")
              (structural-editing-mcp.edit:unwrap-node tree path))
            ((equal action "promote")
              (structural-editing-mcp.edit:promote-node tree path))
            (t (error "Unknown action: ~A" action))))
    (format-mutation-result
      (fmt "Successfully executed ~A at ~A" action path)
      path
      structural-editing-mcp.workspace:*workspace-tree*)))

(defun resolve-split-index (tgt index)
  "Resolve split index from INDEX argument or last element of TGT."
  (or index (if (null (cdr tgt)) 0 (lastcar tgt))))

(defun perform-relocate-action (action src tgt index tree)
  "Dispatch relocation mutation and return the updated tree."
  (cond
    ((or (equal action "move") (equal action "copy"))
      (multiple-value-bind (tgt-parent tgt-idx)
                           (resolve-parent-and-index tree tgt index)
        (if (equal action "move")
          (structural-editing-mcp.edit:move-node tree src tgt-parent tgt-idx)
          (structural-editing-mcp.edit:copy-node tree src tgt-parent tgt-idx))))
    ((equal action "swap")
      (structural-editing-mcp.edit:swap-nodes tree src tgt))
    ((equal action "merge")
      (structural-editing-mcp.edit:merge-nodes tree src tgt))
    ((equal action "split")
      (structural-editing-mcp.edit:split-node tree tgt (resolve-split-index tgt index)))
    (t (error "Unknown action: ~A" action))))

(defun handle-tool-ast-relocate (args)
  "Handle the ast_relocate tool execution."
  (let* ((src (to-list (gethash "source_path" args)))
         (tgt (to-list (gethash "target_path" args)))
         (action (gethash "action" args))
         (index (gethash "index" args)))
    (setf structural-editing-mcp.workspace:*workspace-tree*
          (perform-relocate-action action src tgt index structural-editing-mcp.workspace:*workspace-tree*))
    (format-mutation-result
      (fmt "Successfully executed ~A from ~A to ~A" action src tgt)
      tgt
      structural-editing-mcp.workspace:*workspace-tree*)))

(defun handle-tool-ast-refactor (name path args)
  "Handle ast_search, ast_rename, ast_replace_pattern, ast_extract_variable, ast_extract_function."
  (let ((tree structural-editing-mcp.workspace:*workspace-tree*))
    (cond
      ((equal name "ast_search")
        (let* ((query (gethash "query" args))
               (results (perform-search tree path query)))
          (if results
            (fmt "Found ~A matches. Paths:~%~{~A~^~%~}" (length results) results)
            (fmt "No matches found for '~A' at path ~A" query path))))
      ((equal name "ast_rename")
        (let ((old (gethash "old_name" args))
              (new (gethash "new_name" args)))
          (setf structural-editing-mcp.workspace:*workspace-tree*
                (perform-rename tree path old new))
          (fmt "Successfully renamed all occurrences of '~A' to '~A'." old new)))
      ((equal name "ast_replace_pattern")
        (let ((pat (gethash "pattern" args))
              (rep (gethash "replacement" args)))
          (setf structural-editing-mcp.workspace:*workspace-tree*
                (structural-editing-mcp.refactor:replace-pattern tree pat rep))
          (fmt "Successfully executed pattern replacement across workspace.")))
      ((equal name "ast_extract_variable")
        (let ((var-name (gethash "variable_name" args)))
          (setf structural-editing-mcp.workspace:*workspace-tree*
                (structural-editing-mcp.refactor:extract-variable tree path var-name))
          (fmt "Successfully extracted node at ~A into variable '~A'." path var-name)))
      ((equal name "ast_extract_function")
        (let ((func-name (gethash "function_name" args))
              (fn-params (to-list (gethash "params" args))))
          (setf structural-editing-mcp.workspace:*workspace-tree*
                (structural-editing-mcp.refactor:extract-function tree path func-name :params fn-params))
          (fmt "Successfully extracted node at ~A into function '~A'." path func-name))))))

(defun run-tool-lint (tree path dialect args)
  "Run AST linter and return formatted findings."
  (let* ((rules (to-list (gethash "rules" args)))
         (findings (structural-editing-mcp.analysis:lint-ast tree :path path :dialect dialect :rules rules)))
    (structural-editing-mcp.analysis:format-lint-findings findings)))

(defun run-tool-complexity (tree path dialect args)
  "Run complexity analyzer and return formatted report."
  (let* ((min-cc (or (gethash "min_complexity" args) 1))
         (min-depth (or (gethash "min_depth" args) 1))
         (results (structural-editing-mcp.analysis:analyze-complexity tree :path path :dialect dialect
                                                                      :min-complexity min-cc
                                                                      :min-depth min-depth)))
    (structural-editing-mcp.analysis:format-complexity-report results)))

(defun run-tool-duplicates (tree path args)
  "Run duplicate subtree detector and return formatted report."
  (let* ((min-nodes (or (gethash "min_nodes" args) 4))
         (min-depth (or (gethash "min_depth" args) 2))
         (exact (let ((val (gethash "exact" args))) (if (null val) t val)))
         (results (structural-editing-mcp.analysis:find-duplicate-subtrees tree :path path
                                                                           :min-nodes min-nodes
                                                                           :min-depth min-depth
                                                                           :exact exact)))
    (structural-editing-mcp.analysis:format-duplicate-report results)))

(defun run-tool-bindings (tree path dialect args)
  "Run variable binding analysis and return formatted report."
  (let* ((inc-unused (let ((val (gethash "include_unused" args))) (if (null val) t val)))
         (inc-shadowed (let ((val (gethash "include_shadowed" args))) (if (null val) t val)))
         (findings (structural-editing-mcp.analysis:analyze-bindings tree :path path
                                                                     :include-unused inc-unused
                                                                     :include-shadowed inc-shadowed
                                                                     :dialect dialect)))
    (structural-editing-mcp.analysis:format-binding-report findings)))

(defun run-tool-suggestions (tree path dialect args)
  "Run refactoring suggestion aggregator and return formatted suggestions."
  (let* ((min-p (or (gethash "min_priority" args) "low"))
         (cats (to-list (gethash "categories" args)))
         (suggestions (structural-editing-mcp.analysis:suggest-refactorings tree :path path
                                                                            :min-priority min-p
                                                                            :categories cats
                                                                            :dialect dialect)))
    (structural-editing-mcp.analysis:format-refactoring-suggestions suggestions)))

(defun handle-tool-ast-analysis (name path args dialect)
  "Handle static analysis and refactoring suggestion tools."
  (let ((tree structural-editing-mcp.workspace:*workspace-tree*)
        (eff-dialect (or dialect structural-editing-mcp.parser:*current-dialect*)))
    (cond
      ((equal name "ast_lint")
        (run-tool-lint tree path dialect args))
      ((equal name "ast_complexity_metrics")
        (run-tool-complexity tree path dialect args))
      ((equal name "ast_find_duplicates")
        (run-tool-duplicates tree path args))
      ((equal name "ast_analyze_bindings")
        (run-tool-bindings tree path eff-dialect args))
      ((equal name "ast_suggest_refactorings")
        (run-tool-suggestions tree path eff-dialect args)))))

(defparameter *mutation-tools*
  '("ast_modify" "ast_remove" "ast_relocate" "ast_rename"
    "ast_replace_pattern" "ast_extract_variable" "ast_extract_function")
  "List of tool names that mutate the AST.")

(defun mutation-tool-p (name)
  "Return T if tool NAME mutates workspace AST state."
  (member name *mutation-tools* :test #'string=))

(defun dispatch-tool-call (name path args dialect &optional (agent-id "default"))
  "Dispatch tool invocation by tool NAME to appropriate handler."
  (cond
    ((equal name "read_node")
      (handle-tool-read-node path args agent-id))
    ((equal name "ast_modify")
      (handle-tool-ast-modify path args))
    ((equal name "ast_remove")
      (handle-tool-ast-remove path args))
    ((equal name "ast_relocate")
      (handle-tool-ast-relocate args))
    ((member name '("ast_search" "ast_rename" "ast_replace_pattern"
                    "ast_extract_variable" "ast_extract_function") :test #'string=)
      (handle-tool-ast-refactor name path args))
    ((member name '("ast_lint" "ast_complexity_metrics" "ast_find_duplicates"
                    "ast_analyze_bindings" "ast_suggest_refactorings") :test #'string=)
      (handle-tool-ast-analysis name path args dialect))
    ((equal name "commit_workspace")
      (structural-editing-mcp.workspace:write-workspace)
      (fmt "Workspace committed to disk successfully.~%TIP: Remember to run bash test/verification commands to confirm your changes compile and pass tests!"))
    (t
      (error "Tool not found: ~A" name))))

(defun send-tool-error-response (id message error-code error-type &optional extra-fields)
  "Send a standardized JSON-RPC error response for tool execution."
  (let ((payload (dict "content" (list (dict "type" "text" "text" message))
                       "errorCode" error-code
                       "errorType" error-type
                       "isError" t)))
    (when extra-fields
      (loop for (k v) on extra-fields by #'cddr do
        (setf (gethash k payload) v)))
    (send-result id payload)))

(defun execute-mutation-tool-locked (name path args dialect agent-id)
  "Execute a mutation tool holding *WORKSPACE-LOCK* with OCC validation and rollback on error."
  (bt:with-lock-held (structural-editing-mcp.workspace:*workspace-lock*)
    (let ((target-path (if (equal name "ast_relocate")
                         (to-list (href args "target_path"))
                         path)))
      (structural-editing-mcp.workspace:validate-agent-edit
        :agent-id agent-id
        :target-path target-path)
      (let ((old-rev structural-editing-mcp.workspace:*workspace-revision*)
            (old-tree structural-editing-mcp.workspace:*workspace-tree*)
            (old-agent-rev (gethash agent-id structural-editing-mcp.workspace:*agent-views*)))
        (structural-editing-mcp.workspace:commit-agent-edit agent-id)
        (handler-case
            (dispatch-tool-call name path args dialect agent-id)
          (error (e)
            (setf structural-editing-mcp.workspace:*workspace-tree* old-tree)
            (setf structural-editing-mcp.workspace:*workspace-revision* old-rev)
            (if old-agent-rev
              (setf (gethash agent-id structural-editing-mcp.workspace:*agent-views*) old-agent-rev)
              (remhash agent-id structural-editing-mcp.workspace:*agent-views*))
            (error e)))))))

(defun execute-tool-call (name path args dialect agent-id)
  "Route tool execution based on whether it requires mutation locking or plain dispatch."
  (cond
    ((mutation-tool-p name)
      (execute-mutation-tool-locked name path args dialect agent-id))
    ((equal name "commit_workspace")
      (bt:with-lock-held (structural-editing-mcp.workspace:*workspace-lock*)
        (dispatch-tool-call name path args dialect agent-id)))
    (t
      (dispatch-tool-call name path args dialect agent-id))))

(defun handle-tools-call (id params)
  (let* ((name (href params "name"))
         (args (href params "arguments"))
         (path (to-list (href args "path")))
         (dialect (parse-dialect-arg (href args "dialect")))
         (agent-id (or (href args "agent_id") (href args "agent") "default"))
         (ws-id (or (href args "workspace_id") (href args "workspace") "default")))
    (handler-case
        (let* ((ws (structural-editing-mcp.workspace:get-workspace ws-id))
               (content (structural-editing-mcp.workspace:with-workspace-context (ws)
                          (execute-tool-call name path args dialect agent-id))))
          (send-result id (dict "content" (list (dict "type" "text" "text" content)))))
      (structural-editing-mcp.conditions:occ-conflict-error (c)
        (send-tool-error-response
          id
          (format nil "~A" c)
          structural-editing-mcp.conditions:+error-code-occ-conflict+
          "occ_conflict"))
      (structural-editing-mcp.conditions:invalid-path-error (c)
        (send-tool-error-response
          id
          (format nil "Invalid path error: ~A" c)
          structural-editing-mcp.conditions:+error-code-invalid-params+
          "invalid_path"
          (list "path" (structural-editing-mcp.conditions:invalid-path-error-path c))))
      (structural-editing-mcp.conditions:sexp-parse-error (c)
        (send-tool-error-response
          id
          (format nil "Parse error: ~A" c)
          structural-editing-mcp.conditions:+error-code-parse-error+
          "parse_error"))
      (structural-editing-mcp.conditions:workspace-error (c)
        (send-tool-error-response
          id
          (format nil "Workspace error: ~A" c)
          structural-editing-mcp.conditions:+error-code-workspace-error+
          "workspace_error"))
      (error (e)
        (send-tool-error-response
          id
          (fmt "Error: ~A" e)
          structural-editing-mcp.conditions:+error-code-internal-error+
          "internal_error")))))


(defun handle-message (msg)
  "Dispatch a parsed JSON-RPC message."
  (let ((jsonrpc (href msg "jsonrpc"))
        (id (href msg "id"))
        (method (href msg "method"))
        (params (href msg "params")))
    (unless (equal jsonrpc "2.0") (return-from handle-message nil))
    (cond
      ((equal method "initialize") (handle-initialize id params))
      ((equal method "notifications/initialized")
        ;; No response needed
        nil)
      ((equal method "tools/list") (handle-tools-list id params))
      ((equal method "tools/call") (handle-tools-call id params))
      (id (send-error id -32601 (fmt "Method not found: ~A" method))))))

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
                    (enqueue-task (lambda () (handle-message msg))))
                (error (e)
                  (format *error-output* "Parse error: ~A~%" e)
                  (force-output *error-output*)))))))
    (stop-worker-pool)))