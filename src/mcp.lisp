(defpackage :structural-editing-mcp.mcp
  (:use :cl
        :alexandria
        :trivia
        :structural-editing-mcp.utils
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.edit
        :structural-editing-mcp.analysis
        :structural-editing-mcp.workspace)
  (:export :start-server :handle-message))

(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

;;; JSON-RPC & MCP Utilities


(defun to-list (val)
  "Ensure val is a list, converting from vector if necessary."
  (if (vectorp val) (coerce val 'list) val))

(defun dict (&rest keys-and-values)
  "Create a hash-table dictionary from key-value pairs."
  (let
    ((ht (make-hash-table :test 'equal)))
    (loop for (k v) on keys-and-values by #'cddr do (setf (gethash k ht) v))
    ht))

(defun send-json (object)
  "Encode and send JSON over stdout."
  (yason:encode object *standard-output*)
  (terpri *standard-output*)
  (force-output *standard-output*))

(defun send-error (id code message)
  (send-json
             (dict "jsonrpc" "2.0" "id" id "error" (dict "code" code "message" message))))

(defun send-result (id result)
  (send-json (dict "jsonrpc" "2.0" "id" id "result" result)))

;;; Node Presentation & Discovery Helpers


(defun print-children-tree (s node current-depth max-depth base-path)
  (when
        (and
         (< current-depth max-depth)
         (not
           (member (structural-editing-mcp.tree:get-node-tag node) ' (:leaf :comment))))
        (let
      ((children (structural-editing-mcp.tree:get-node-children node)))
      (when
            children
            (loop
              for
              child
              in
              children
              for
              idx
              from
              0
              for
              cpath
              =
              (or
              (structural-editing-mcp.tree:get-node-path child)
              (append base-path (list idx)))
              for
              ctag
              =
              (structural-editing-mcp.tree:get-node-tag child)
              for
              raw-str
              =
              (structural-editing-mcp.parser:sexp-to-string child)
              for
              single-line
              =
              (substitute #\space #\newline (string-trim ' (#\space #\newline) raw-str))
              for
              snippet
              =
              (if
              (> (length single-line) 70)
              (format nil "~A..." (subseq single-line 0 67))
              single-line)
              for
              indent
              =
              (make-string (* (1+ current-depth) 2) :initial-element #\space)
              do
              (format s "~A[~{~A~^, ~}] ~A: ~A~%" indent cpath ctag snippet)
              (print-children-tree s child (1+ current-depth) max-depth cpath))))))

(defun format-node-preview (node &key (depth 2))
  "Format a node with its path, tag, rendered code, and summary of children up to DEPTH."
  (if
      (null node)
      "Node not found at given path."
      (let*
      ((path (structural-editing-mcp.tree:get-node-path node))
       (tag (structural-editing-mcp.tree:get-node-tag node))
       (children (structural-editing-mcp.tree:get-node-children node))
       (code (structural-editing-mcp.parser:sexp-to-string node)))
      (with-output-to-string
        (s)
        (format s "Path: ~A~%" (or path "()"))
        (format s "Tag: ~A~%" tag)
        (cond
          ((eq tag :workspace)
           (if (null children)
               (format s "Workspace is empty. Provide load_files in read_node to load files into the workspace.~%")
               (progn
                 (format s "Active Dialects (~A):~%" (length children))
                 (loop for dialect-node in children
                       for d-idx from 0
                       for d-path = (or (structural-editing-mcp.tree:get-node-path dialect-node) (list d-idx))
                       for d-tag = (structural-editing-mcp.tree:get-node-tag dialect-node)
                       for file-nodes = (structural-editing-mcp.tree:get-node-children dialect-node)
                       do
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
                                     f-path (or filepath "unknown") form-count))))))
          ((member tag structural-editing-mcp.workspace:*known-dialects*)
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
          ((eq tag :file)
           (let ((filepath (structural-editing-mcp.workspace:get-filepath path)))
             (when filepath (format s "File: ~A~%" filepath)))
           (format s "Code:~%~A~%" code)
           (when
                  children
                  (format s "~%Children (~A top-level forms):~%" (length children))
                  (loop
                    for
                    child
                    in
                    children
                    for
                    idx
                    from
                    0
                    for
                    cpath
                    =
                    (or (structural-editing-mcp.tree:get-node-path child) (append path (list idx)))
                    for
                    ctag
                    =
                    (structural-editing-mcp.tree:get-node-tag child)
                    for
                    raw-str
                    =
                    (structural-editing-mcp.parser:sexp-to-string child)
                    for
                    single-line
                    =
                    (substitute #\space #\newline (string-trim ' (#\space #\newline) raw-str))
                    for
                    snippet
                    =
                    (if
                    (> (length single-line) 70)
                    (format nil "~A..." (subseq single-line 0 67))
                    single-line)
                    do
                    (format s "  [~{~A~^, ~}] ~A: ~A~%" cpath ctag snippet)
                    (when (> depth 1) (print-children-tree s child 1 depth cpath)))))
          (t
             (format s "Code:~%~A~%" code)
             (when
                  children
                  (format s "~%Children (~A):~%" (length children))
                  (loop
                    for
                    child
                    in
                    children
                    for
                    idx
                    from
                    0
                    for
                    cpath
                    =
                    (or (structural-editing-mcp.tree:get-node-path child) (append path (list idx)))
                    for
                    ctag
                    =
                    (structural-editing-mcp.tree:get-node-tag child)
                    for
                    raw-str
                    =
                    (structural-editing-mcp.parser:sexp-to-string child)
                    for
                    single-line
                    =
                    (substitute #\space #\newline (string-trim ' (#\space #\newline) raw-str))
                    for
                    snippet
                    =
                    (if
                    (> (length single-line) 70)
                    (format nil "~A..." (subseq single-line 0 67))
                    single-line)
                    do
                    (format s "  [~{~A~^, ~}] ~A: ~A~%" cpath ctag snippet)
                    (when (> depth 1) (print-children-tree s child 1 depth cpath))))))))))

;;; AST Mutation Dispatchers


(defun perform-wrap (tree path wrapper-str &optional end-index index)
  "Wrap the node at PATH, or range of nodes from START-INDEX to END-INDEX under PARENT-PATH."
  (if (null end-index)
      ;; Single node wrap
      (let* ((clean-str (string-trim '(#\Space #\Tab #\Newline #\:) (or wrapper-str "")))
             (lower (string-downcase clean-str)))
        (cond
          ((or (string= lower "paren") (string= lower "()") (string= lower ""))
           (structural-editing-mcp.edit:wrap-node tree path :paren))
          ((or (string= lower "square") (string= lower "bracket") (string= lower "[]"))
           (structural-editing-mcp.edit:wrap-node tree path :square))
          ((or (string= lower "curly") (string= lower "brace") (string= lower "{}"))
           (structural-editing-mcp.edit:wrap-node tree path :curly))
          (t
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
                 (structural-editing-mcp.edit:wrap-node tree path :paren))))))
      ;; Range wrap
      (let* ((parent-path (if index path (if (null (cdr path)) path (butlast path))))
             (start-idx (if index index (if (null (cdr path)) 0 (lastcar path))))
             (clean-str (string-trim '(#\Space #\Tab #\Newline #\:) (or wrapper-str "")))
             (lower (string-downcase clean-str)))
        (cond
          ((or (string= lower "paren") (string= lower "()") (string= lower ""))
           (structural-editing-mcp.edit:wrap-range tree parent-path start-idx end-index :paren))
          ((or (string= lower "square") (string= lower "bracket") (string= lower "[]"))
           (structural-editing-mcp.edit:wrap-range tree parent-path start-idx end-index :square))
          ((or (string= lower "curly") (string= lower "brace") (string= lower "{}"))
           (structural-editing-mcp.edit:wrap-range tree parent-path start-idx end-index :curly))
          (t
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
                 (structural-editing-mcp.edit:wrap-range tree parent-path start-idx end-index :paren))))))))


(defun perform-insert (tree path new-node-str &optional index)
  "Insert NEW-NODE-STR into TREE. If INDEX is provided, PATH is the parent.
Otherwise, PATH specifies the target location (parent is (butlast path), index is (lastcar path))."
  (let*
    ((has-explicit-index (not (null index)))
     (parent-path
                   (if has-explicit-index path (if (null (cdr path)) path (butlast path))))
     (target-idx
                  (if
            has-explicit-index
            index
            (if
              (null (cdr path))
              (length
                    (structural-editing-mcp.tree:get-node-children
                (structural-editing-mcp.tree:get-node-at-path tree path)))
              (lastcar path)))))
    (structural-editing-mcp.edit:insert-expression
      tree
      parent-path
      target-idx
      new-node-str)))

(defun resolve-parent-and-index (tree target-path &optional index)
  "Resolve target parent path and index for move or copy."
  (if
      index
      (values target-path index)
      (if
        (null (cdr target-path))
        (values
              target-path
              (length
                (structural-editing-mcp.tree:get-node-children
            (structural-editing-mcp.tree:get-node-at-path tree target-path))))
        (values (butlast target-path) (lastcar target-path)))))

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
    (if
        preview-node
        (format
              nil
              "~A~%~%Updated preview at ~A:~%~A"
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


(defun get-tools-list ()
  (list
        (dict
          "name"
          "read_node"
          "description"
          "Inspect any node in the AST or workspace. Returns rendered code and a nested tree of child paths up to 'depth' levels. BEST PRACTICE: Always read parent forms (e.g. [0, 10]) to see the entire expression and its child paths at once—do NOT probe child indices one-by-one. Use 'ast_search' to find symbols/calls across the workspace."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "0-indexed array of integers specifying the AST path. Omit or pass [] for the workspace root, [0] for dialect 0 (e.g. :common-lisp), [0, 0] for file 0 in dialect 0, [0, 0, 2] for top-level form 2 in file 0, [0, 0, 3, 1] for child 1 of form 3.")
              "depth"
              (dict
                "type"
                "integer"
                "description"
                "Recursion depth for displaying nested children and their paths (default: 2). Use depth 1 for only immediate children, 2 or 3 to inspect deeper sub-expressions.")
              "load_files"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "string")
                "description"
                "Optional list of file or directory paths to load into the workspace (e.g. ['/path/to/project'] or ['/path/to/file.lisp']). Directories are recursively scanned for Lisp source files."))))
        (dict
          "name"
          "ast_modify"
          "description"
          "Mutate AST nodes. Actions: 'insert' (adds new_node before path, or at child index if index is given), 'overwrite' (replaces node at path with new_node), 'wrap' (wraps node at path with parens, brackets, or an enclosing form). NOTE: Automatically returns an updated preview of the enclosing parent node; separate verification reads are unnecessary."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "Target AST path (e.g. [0, 2] to target form 2 in file 0).")
              "action"
              (dict
                "type"
                "string"
                "enum"
                (list "insert" "overwrite" "wrap")
                "description"
                "The modification action to perform.")
              "new_node"
              (dict
                "type"
                "string"
                "description"
                "For insert/overwrite: the S-expression code string (e.g. '(defun foo () 42)'). For wrap: delimiter keyword (':paren', ':square', ':curly') or enclosing form string (e.g. '(when condition)').")
              "index"
              (dict
                "type"
                "integer"
                "description"
                "Optional child index for insert. If omitted when inserting, uses the last element of path.")
              "end_index"
              (dict
                "type"
                "integer"
                "description"
                "Optional ending child index for range wrapping with action 'wrap'."))
            "required"
            (list "path" "action" "new_node")))
        (dict
          "name"
          "ast_remove"
          "description"
          "Remove or unwrap AST nodes. Actions: 'delete' (deletes the node at path), 'unwrap' (removes enclosing collection, spilling children into parent), 'promote' (replaces parent node with the node at path). NOTE: Automatically returns an updated preview."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "The AST path of the node to remove/unwrap/promote.")
              "action"
              (dict
                "type"
                "string"
                "enum"
                (list "delete" "unwrap" "promote")
                "description"
                "Removal action to perform."))
            "required"
            (list "path" "action")))
        (dict
          "name"
          "ast_relocate"
          "description"
          "Move, copy, swap, merge, or split AST nodes. Actions: 'move' (moves source_path to target_path), 'copy' (duplicates source_path to target_path), 'swap' (swaps nodes at source_path and target_path), 'merge' (merges sibling collection nodes), 'split' (splits target collection node at index). NOTE: Automatically returns an updated preview."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "source_path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "Path to source node (optional for split).")
              "target_path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "Target path for move/copy/swap/merge/split (e.g. [0, 2] places before index 2 in file 0).")
              "action"
              (dict
                "type"
                "string"
                "enum"
                (list "move" "copy" "swap" "merge" "split")
                "description"
                "Relocation action to perform.")
              "index"
              (dict
                "type"
                "integer"
                "description"
                "Optional target child index for move/copy/split."))
            "required"
            (list "target_path" "action")))
        (dict
          "name"
          "ast_search"
          "description"
          "FAST SEARCH: Find all occurrences of a symbol, function name, or keyword across the entire workspace or under a specific path. Returns exact AST paths to each match without needing to walk the tree manually."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "query"
              (dict
                "type"
                "string"
                "description"
                "The symbol or text to search for (e.g. 'make-api-call').")
              "path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "Optional AST path to constrain the search to a specific node/file. If omitted, searches the entire workspace."))
            "required"
            (list "query")))
        (dict
          "name"
          "ast_rename"
          "description"
          "Bulk rename/replace a specific leaf node symbol anywhere in the workspace or under a specific AST path."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "old_name"
              (dict
                "type"
                "string"
                "description"
                "The exact symbol/string to replace (e.g. 'make-api-call').")
              "new_name"
              (dict
                "type"
                "string"
                "description"
                "The new symbol/string to replace it with (e.g. 'execute-api-call').")
              "path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "Optional AST path to constrain the bulk rename to a specific subtree. If omitted, renames globally across the workspace."))
            "required"
            (list "old_name" "new_name")))
        (dict
          "name"
          "ast_replace_pattern"
          "description"
          "Search the workspace for a structural Lisp pattern and replace it with a new pattern, preserving matched variables (e.g. pattern='(foo ?x ?y)', replacement='(bar ?y ?x)')."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "pattern"
              (dict
                "type"
                "string"
                "description"
                "The pattern to match. Variables start with '?' (e.g. '(make-api-call ?method ?url ?headers ?body)').")
              "replacement"
              (dict
                "type"
                "string"
                "description"
                "The replacement template (e.g. '(make-api-call ?url ?method :headers ?headers :body ?body)')."))
            "required"
            (list "pattern" "replacement")))
        (dict
          "name"
          "ast_extract_variable"
          "description"
          "Extracts an AST node into a local `let` binding wrapped around its immediate parent."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "AST path of the node to extract.")
              "variable_name"
              (dict
                "type"
                "string"
                "description"
                "The name of the new variable to bind it to."))
            "required"
            (list "path" "variable_name")))
        (dict
          "name"
          "ast_extract_function"
          "description"
          "Extracts an AST node into a new top-level function definition and replaces the original node with a call to the new function."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "AST path of the node to extract into a function.")
              "function_name"
              (dict
                "type"
                "string"
                "description"
                "The name of the new function.")
              "params"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "string")
                "description"
                "Optional list of parameter names for the new function."))
            "required"
            (list "path" "function_name")))
        (dict
          "name"
          "ast_lint"
          "description"
          "Run static analysis and structural linting to identify code smells, anti-patterns, and opportunities for refactoring. Returns a list of findings with AST paths, messages, severity, and suggested quick-fixes."
          "inputSchema"
          (dict
            "type"
            "object"
            "properties"
            (dict
              "path"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "integer")
                "description"
                "Optional AST path to lint a specific node or file. If omitted, lints the entire workspace.")
              "dialect"
              (dict
                "type"
                "string"
                "description"
                "Optional Lisp dialect override (:common-lisp, :clojure, :scheme, :emacs-lisp, :fennel). Inferred if omitted.")
              "rules"
              (dict
                "type"
                "array"
                "items"
                (dict "type" "string")
                "description"
                "Optional list of rule IDs to filter by (e.g. ['if-progn-to-when', 'single-clause-cond'])."))))
        (dict
          "name"
          "commit_workspace"
          "description"
          "Persists all in-memory workspace modifications back to their respective files on disk."
          "inputSchema"
          (dict "type" "object" "properties" (make-hash-table)))))

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
          (dict "name" "structural-editing-mcp" "version" "0.1.0"))))

(defun handle-tools-list (id params)
  (declare (ignore params))
  (send-result id (dict "tools" (get-tools-list))))

(defun handle-tools-call (id params)
  (let
    ((name (gethash "name" params)) (args (gethash "arguments" params)))
    (handler-case
                  (let
        ((content
                   (cond
              ((equal name "read_node")
               (let
                  ((files-to-load (to-list (gethash "load_files" args))))
                  (when
                        files-to-load
                        (structural-editing-mcp.workspace:load-into-workspace files-to-load)))
               (unless
                        structural-editing-mcp.workspace:*workspace-tree*
                        (structural-editing-mcp.workspace:init-workspace))
               (let*
                  ((path (to-list (gethash "path" args)))
                   (depth (or (gethash "depth" args) 2))
                   (node
                          (if
                          (null path)
                          structural-editing-mcp.workspace:*workspace-tree*
                          (structural-editing-mcp.tree:get-node-at-path
                          structural-editing-mcp.workspace:*workspace-tree*
                          path))))
                  (format-node-preview node :depth depth)))
              ((equal name "ast_modify")
               (let*
                  ((path (to-list (gethash "path" args)))
                   (action (gethash "action" args))
                   (new-node-str (gethash "new_node" args))
                   (index (gethash "index" args))
                   (end-index (gethash "end_index" args)))
                  (cond
                    ((equal action "insert")
                     (setf
                            structural-editing-mcp.workspace:*workspace-tree*
                            (perform-insert
                                        structural-editing-mcp.workspace:*workspace-tree*
                                        path
                                        new-node-str
                                        index)))
                    ((equal action "overwrite")
                     (setf
                            structural-editing-mcp.workspace:*workspace-tree*
                            (structural-editing-mcp.edit:overwrite-expression
                          structural-editing-mcp.workspace:*workspace-tree*
                          path
                          new-node-str)))
                    ((equal action "wrap")
                     (setf
                            structural-editing-mcp.workspace:*workspace-tree*
                            (perform-wrap
                                      structural-editing-mcp.workspace:*workspace-tree*
                                      path
                                      new-node-str
                                      end-index
                                      index)))
                    (t (error "Unknown action: ~A" action)))
                  (format-mutation-result
                    (format nil "Successfully executed ~A at ~A" action path)
                    path
                    structural-editing-mcp.workspace:*workspace-tree*)))
              ((equal name "ast_remove")
               (let*
                  ((path (to-list (gethash "path" args))) (action (gethash "action" args)))
                  (cond
                    ((equal action "delete")
                     (setf
                            structural-editing-mcp.workspace:*workspace-tree*
                            (structural-editing-mcp.edit:delete-node
                          structural-editing-mcp.workspace:*workspace-tree*
                          path)))
                    ((equal action "unwrap")
                     (setf
                            structural-editing-mcp.workspace:*workspace-tree*
                            (structural-editing-mcp.edit:unwrap-node
                          structural-editing-mcp.workspace:*workspace-tree*
                          path)))
                    ((equal action "promote")
                     (setf
                            structural-editing-mcp.workspace:*workspace-tree*
                            (structural-editing-mcp.edit:promote-node
                          structural-editing-mcp.workspace:*workspace-tree*
                          path)))
                    (t (error "Unknown action: ~A" action)))
                  (format-mutation-result
                    (format nil "Successfully executed ~A at ~A" action path)
                    path
                    structural-editing-mcp.workspace:*workspace-tree*)))
              ((equal name "ast_relocate")
               (let*
                  ((src (to-list (gethash "source_path" args)))
                   (tgt (to-list (gethash "target_path" args)))
                   (action (gethash "action" args))
                   (index (gethash "index" args)))
                  (cond
                    ((equal action "move")
                     (multiple-value-bind
                        (tgt-parent tgt-idx)
                        (resolve-parent-and-index
                          structural-editing-mcp.workspace:*workspace-tree*
                          tgt
                          index)
                        (setf
                               structural-editing-mcp.workspace:*workspace-tree*
                               (structural-editing-mcp.edit:move-node
                             structural-editing-mcp.workspace:*workspace-tree*
                             src
                             tgt-parent
                             tgt-idx))))
                    ((equal action "copy")
                     (multiple-value-bind
                        (tgt-parent tgt-idx)
                        (resolve-parent-and-index
                          structural-editing-mcp.workspace:*workspace-tree*
                          tgt
                          index)
                        (setf
                               structural-editing-mcp.workspace:*workspace-tree*
                               (structural-editing-mcp.edit:copy-node
                             structural-editing-mcp.workspace:*workspace-tree*
                             src
                             tgt-parent
                             tgt-idx))))
                    ((equal action "swap")
                     (setf
                            structural-editing-mcp.workspace:*workspace-tree*
                            (structural-editing-mcp.edit:swap-nodes
                          structural-editing-mcp.workspace:*workspace-tree*
                          src
                          tgt)))
                    ((equal action "merge")
                     (setf
                            structural-editing-mcp.workspace:*workspace-tree*
                            (structural-editing-mcp.edit:merge-nodes
                          structural-editing-mcp.workspace:*workspace-tree*
                          src
                          tgt)))
                    ((equal action "split")
                     (let ((split-idx (or index (if (null (cdr tgt)) 0 (lastcar tgt)))))
                       (setf
                              structural-editing-mcp.workspace:*workspace-tree*
                              (structural-editing-mcp.edit:split-node
                            structural-editing-mcp.workspace:*workspace-tree*
                            tgt
                            split-idx))))
                    (t (error "Unknown action: ~A" action)))
                  (format-mutation-result
                    (format nil "Successfully executed ~A from ~A to ~A" action src tgt)
                    tgt
                    structural-editing-mcp.workspace:*workspace-tree*)))
              ((equal name "ast_search")
               (let*
                  ((path (to-list (gethash "path" args)))
                   (query (gethash "query" args))
                   (results
                              (perform-search structural-editing-mcp.workspace:*workspace-tree* path query)))
                  (if
                      results
                      (format nil "Found ~A matches. Paths:~%~{~A~^~%~}" (length results) results)
                      (format nil "No matches found for '~A' at path ~A" query path))))
              ((equal name "ast_rename")
               (let*
                  ((path (to-list (gethash "path" args)))
                   (old (gethash "old_name" args))
                   (new (gethash "new_name" args)))
                  (setf
                        structural-editing-mcp.workspace:*workspace-tree*
                        (perform-rename structural-editing-mcp.workspace:*workspace-tree* path old new))
                  (format nil "Successfully renamed all occurrences of '~A' to '~A'." old new)))
              ((equal name "ast_replace_pattern")
               (let
                  ((pat (gethash "pattern" args)) (rep (gethash "replacement" args)))
                  (setf
                        structural-editing-mcp.workspace:*workspace-tree*
                        (structural-editing-mcp.refactor:replace-pattern
                      structural-editing-mcp.workspace:*workspace-tree*
                      pat
                      rep))
                  (format nil "Successfully executed pattern replacement across workspace.")))
              ((equal name "ast_extract_variable")
               (let
                  ((path (to-list (gethash "path" args)))
                   (var-name (gethash "variable_name" args)))
                  (setf
                        structural-editing-mcp.workspace:*workspace-tree*
                        (structural-editing-mcp.refactor:extract-variable
                      structural-editing-mcp.workspace:*workspace-tree*
                      path
                      var-name))
                  (format
                          nil
                          "Successfully extracted node at ~A into variable '~A'."
                          path
                          var-name)))
              ((equal name "ast_extract_function")
               (let
                  ((path (to-list (gethash "path" args)))
                   (func-name (gethash "function_name" args))
                   (params (to-list (gethash "params" args))))
                  (setf
                        structural-editing-mcp.workspace:*workspace-tree*
                        (structural-editing-mcp.refactor:extract-function
                      structural-editing-mcp.workspace:*workspace-tree*
                      path
                      func-name
                      :params params))
                  (format
                          nil
                          "Successfully extracted node at ~A into function '~A'."
                          path
                          func-name)))
              ((equal name "ast_lint")
               (let* ((path (to-list (gethash "path" args)))
                      (dialect-str (gethash "dialect" args))
                      (dialect (when (and dialect-str (plusp (length dialect-str)))
                                 (intern (string-upcase (string-left-trim ":" dialect-str)) :keyword)))
                      (rules (to-list (gethash "rules" args)))
                      (findings (structural-editing-mcp.analysis:lint-ast
                                 structural-editing-mcp.workspace:*workspace-tree*
                                 :path path
                                 :dialect dialect
                                 :rules rules)))
                 (structural-editing-mcp.analysis:format-lint-findings findings)))
              ((equal name "commit_workspace")
               (structural-editing-mcp.workspace:write-workspace)
               (format
                        nil
                        "Workspace committed to disk successfully.~%TIP: Remember to run bash test/verification commands to confirm your changes compile and pass tests!"))
              (t (error "Tool not found: ~A" name)))))

        (send-result id (dict "content" (list (dict "type" "text" "text" content)))))
                  (error
             (e)
             (send-result
                     id
                     (dict
                "content"
                (list (dict "type" "text" "text" (format nil "Error: ~A" e)))
                "isError"
                t))))))

(defun handle-message (msg)
  "Dispatch a parsed JSON-RPC message."
  (let
    ((jsonrpc (gethash "jsonrpc" msg))
     (id (gethash "id" msg))
     (method (gethash "method" msg))
     (params (gethash "params" msg)))
    (unless (equal jsonrpc "2.0") (return-from handle-message nil))
    (cond
      ((equal method "initialize") (handle-initialize id params))
      ((equal method "notifications/initialized")
       ;; No response needed

       nil)
      ((equal method "tools/list") (handle-tools-list id params))
      ((equal method "tools/call") (handle-tools-call id params))
      (id (send-error id -32601 (format nil "Method not found: ~A" method))))))

(defun start-server ()
  "Start the MCP server loop over stdin/stdout."
  (structural-editing-mcp.workspace:init-workspace)
  (let
    ((yason:*parse-json-arrays-as-vectors* nil))
    (loop
          (let
        ((line (read-line *standard-input* nil :eof)))
        (when (eq line :eof) (return))
        (when
              (plusp (length line))
              (handler-case
                        (let ((msg (yason:parse line))) (handle-message msg))
                        (error
                   (e)
                   (format *error-output* "Parse error: ~A~%" e)
                   (force-output *error-output*))))))))