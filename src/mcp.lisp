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
  (let ((yason:*parse-json-arrays-as-vectors* nil))
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
              (force-output *error-output*))))))))

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

(defparameter *skeleton-threshold-children* 50
                                            "Default child count above which format-node-preview uses skeleton mode.")

(defparameter *skeleton-threshold-chars* 3000
                                         "Default character length above which format-node-preview uses skeleton mode.")

(defun count-subtree-nodes (node)
  "Recursively count total AST nodes under NODE."
  (if (null node)
    0
    (let
        ((children (structural-editing-mcp.tree:get-node-children node)))
      (if (null children)
        1
        (1+ (reduce #'+ children :key #'count-subtree-nodes :initial-value 0))))))

(defun format-child-stub (c)
  (let
      ((ctag (structural-editing-mcp.tree:get-node-tag c)))
    (cond
      ((or (eq ctag :leaf) (eq ctag :comment))
        (structural-editing-mcp.parser:sexp-to-string c))
      ((eq ctag :bracket) "[...]")
      ((eq ctag :brace) "{...}")
      (t "(...)"))))

(defun extract-node-signature
       (node &key (max-elements 3) (max-length 60))
  "Extract a compact signature/stub string for a node without hydrating its entire body."
  (if (null node)
    ""
    (let
        ((tag (structural-editing-mcp.tree:get-node-tag node)))
      (cond
        ((or (eq tag :leaf) (eq tag :comment))
          (truncate-preview-string
            (structural-editing-mcp.parser:sexp-to-string node)
            :max-length
            max-length))
        (t
          (let
              ((children (structural-editing-mcp.tree:get-node-children node))
               (prefix
                 (if (eq tag :bracket)
                   "["
                   (if (eq tag :brace)
                     "{"
                     "(")))
               (suffix
                 (if (eq tag :bracket)
                   "]"
                   (if (eq tag :brace)
                     "}"
                     ")"))))
            (if (null children)
              (concatenate 'string prefix suffix)
              (let*
                  ((count (min (length children) max-elements))
                   (head (subseq children 0 count))
                   (has-more (> (length children) max-elements))
                   (items (mapcar #'format-child-stub head))
                   (content (format nil "~{~A~^ ~}" items))
                   (res
                     (if has-more
                       (format nil "~A~A ...~A" prefix content suffix)
                       (format nil "~A~A~A" prefix content suffix))))
                (truncate-preview-string res :max-length max-length)))))))))

(defun print-children-tree
       (s node current-depth max-depth base-path &key skeleton)
  (when
      (and (< current-depth max-depth)
           (not (member (structural-editing-mcp.tree:get-node-tag node) '(:leaf :comment))))
    (let
        ((children (structural-editing-mcp.tree:get-node-children node)))
      (when children
        (loop for
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
              snippet
              =
              (if skeleton
                (extract-node-signature child)
                (truncate-preview-string (structural-editing-mcp.parser:sexp-to-string child)))
              for
              indent
              =
              (make-string (* (1+ current-depth) 2) :initial-element #\space)
              do
              (format s "~A[~{~A~^, ~}] ~A: ~A~%" indent cpath ctag snippet)
              (print-children-tree s
                                   child
                                   (1+ current-depth)
                                   max-depth
                                   cpath
                                   :skeleton
                                   skeleton))))))

(defun format-children-preview
       (s children
        path
        depth
        &optional
        (label-suffix "")
        &key
        (limit 50)
        (offset 0)
        skeleton)
  "Format child nodes with paths, tags, and preview snippets up to DEPTH with optional pagination and skeleton mode."
  (when children
    (let*
        ((total (length children))
         (start (min (max 0 (or offset 0)) total))
         (effective-limit (or limit 50))
         (end (min (+ start effective-limit) total))
         (slice (subseq children start end)))
      (if (or (plusp start) (< end total))
        (format s
                "~%Children (~A~A, showing ~A-~A):~%"
                total
                label-suffix
                (1+ start)
                end)
        (format s "~%Children (~A~A):~%" total label-suffix))
      (loop for
            child
            in
            slice
            for
            idx
            from
            start
            for
            cpath
            =
            (or (structural-editing-mcp.tree:get-node-path child) (append path (list idx)))
            for
            ctag
            =
            (structural-editing-mcp.tree:get-node-tag child)
            for
            snippet
            =
            (if skeleton
              (extract-node-signature child)
              (truncate-preview-string (structural-editing-mcp.parser:sexp-to-string child)))
            do
            (format s "  [~{~A~^, ~}] ~A: ~A~%" cpath ctag snippet)
            (when (> depth 1)
              (print-children-tree s child 1 depth cpath :skeleton skeleton)))
      (when (< end total)
        (format s
                "  ... (~A more child items; use limit and offset in read_node to paginate)~%"
                (- total end))))))

(defun format-dialect-node-preview
       (s dialect-node d-path &key (limit 50) (offset 0))
  "Format preview for a single DIALECT-NODE under D-PATH to stream S with optional pagination."
  (let*
      ((d-tag (structural-editing-mcp.tree:get-node-tag dialect-node))
       (file-nodes (structural-editing-mcp.tree:get-node-children dialect-node))
       (total (length file-nodes))
       (start (min (max 0 (or offset 0)) total))
       (effective-limit (or limit 50))
       (end (min (+ start effective-limit) total))
       (slice (subseq file-nodes start end)))
    (if (or (plusp start) (< end total))
      (format s
              "  [~{~A~^, ~}] ~A (~A file~:P, showing ~A-~A):~%"
              d-path
              d-tag
              total
              (1+ start)
              end)
      (format s "  [~{~A~^, ~}] ~A (~A file~:P):~%" d-path d-tag total))
    (loop for
          file-node
          in
          slice
          for
          f-idx
          from
          start
          for
          f-path
          =
          (or
            (structural-editing-mcp.tree:get-node-path file-node)
            (append d-path (list f-idx)))
          for
          filepath
          =
          (structural-editing-mcp.workspace:get-filepath f-path)
          for
          form-count
          =
          (length (structural-editing-mcp.tree:get-node-children file-node))
          do
          (format s
                  "    [~{~A~^, ~}] :FILE (~A) — ~A top-level forms~%"
                  f-path
                  (or filepath "unknown")
                  form-count))
    (when (< end total)
      (format s
              "    ... (~A more files; use limit and offset in read_node to paginate)~%"
              (- total end)))))

(defun format-workspace-preview
       (s children &key (limit 50) (offset 0))
  "Format preview for workspace root to stream S with pagination."
  (if (null children)
    (format s
            "Workspace is empty. Provide load_files in read_node to load files into the workspace.~%")
    (progn
      (format s "Active Dialects (~A):~%" (length children))
      (loop for
            dialect-node
            in
            children
            for
            d-idx
            from
            0
            for
            d-path
            =
            (or (structural-editing-mcp.tree:get-node-path dialect-node) (list d-idx))
            do
            (format-dialect-node-preview s dialect-node d-path :limit limit :offset offset)))))

(defun format-dialect-preview
       (s children path tag &key (limit 50) (offset 0))
  "Format preview for a dialect partition node to stream S with optional pagination."
  (format s "Dialect: ~A~%" tag)
  (if (null children)
    (format s "No files loaded for this dialect.~%")
    (let*
        ((total (length children))
         (start (min (max 0 (or offset 0)) total))
         (effective-limit (or limit 50))
         (end (min (+ start effective-limit) total))
         (slice (subseq children start end)))
      (if (or (plusp start) (< end total))
        (format s "Files Loaded (~A, showing ~A-~A):~%" total (1+ start) end)
        (format s "Files Loaded (~A):~%" total))
      (loop for
            file-node
            in
            slice
            for
            idx
            from
            start
            for
            f-path
            =
            (or
              (structural-editing-mcp.tree:get-node-path file-node)
              (append path (list idx)))
            for
            filepath
            =
            (structural-editing-mcp.workspace:get-filepath f-path)
            for
            form-count
            =
            (length (structural-editing-mcp.tree:get-node-children file-node))
            do
            (format s
                    "  [~{~A~^, ~}] :FILE (~A) — ~A top-level forms~%"
                    f-path
                    (or filepath "unknown")
                    form-count))
      (when (< end total)
        (format s
                "  ... (~A more files; use limit and offset in read_node to paginate)~%"
                (- total end))))))

(defun format-node-code-and-metrics
       (s node code children skeleton-p)
  "Format either SKELETON metrics or full rendered CODE to stream S."
  (if skeleton-p
    (progn
      (format s "Mode: SKELETON (full code suppressed to prevent context blowout)~%")
      (format s "Metrics:~%")
      (format s "  Direct Child Forms: ~D~%" (length children))
      (format s "  Total Subtree Nodes: ~D~%" (count-subtree-nodes node))
      (format s "  Approx Code Length: ~D characters~%" (length code)))
    (format s "Code:~%~A~%" code)))

(defun format-node-preview
       (node &key (depth 2) (limit 50) (offset 0) (mode "full"))
  "Format a node with its path, tag, rendered code, and summary of children up to DEPTH with pagination. Supports 'skeleton' mode to prevent context blowout on wide nodes."
  (if (null node)
    "Node not found at given path."
    (let*
        ((path (structural-editing-mcp.tree:get-node-path node))
         (tag (structural-editing-mcp.tree:get-node-tag node))
         (children (structural-editing-mcp.tree:get-node-children node))
         (code (structural-editing-mcp.parser:sexp-to-string node))
         (skeleton-p
           (or (equal mode "skeleton")
               (and (not (equal mode "full"))
                    (or
                      (>= (length children) *skeleton-threshold-children*)
                      (> (length code) *skeleton-threshold-chars*)))
               (and (equal mode "full")
                    (or (>= (length children) 200) (> (length code) 20000))))))
      (with-output-to-string (s)
        (format s
                "Workspace Revision: ~D~%"
                structural-editing-mcp.workspace:*workspace-revision*)
        (format s "Path: ~A~%" (or path "()"))
        (format s "Tag: ~A~%" tag)
        (cond
          ((eq tag :workspace)
            (format-workspace-preview s children :limit limit :offset offset))
          ((member tag structural-editing-mcp.workspace:*known-dialects*)
            (format-dialect-preview s children path tag :limit limit :offset offset))
          ((eq tag :file)
            (let
                ((filepath (structural-editing-mcp.workspace:get-filepath path)))
              (when filepath (format s "File: ~A~%" filepath)))
            (format-node-code-and-metrics s node code children skeleton-p)
            (format-children-preview s
                                     children
                                     path
                                     depth
                                     " top-level forms"
                                     :limit
                                     limit
                                     :offset
                                     offset
                                     :skeleton
                                     skeleton-p))
          (t
            (format-node-code-and-metrics s node code children skeleton-p)
            (format-children-preview s
                                     children
                                     path
                                     depth
                                     ""
                                     :limit
                                     limit
                                     :offset
                                     offset
                                     :skeleton
                                     skeleton-p)))))))

;;; AST Mutation Dispatchers

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
        (lambda (node)
          (match expr
                 ((node p tag children) ` (:path ,p ,tag ,@children ,node)))))
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
  (match action
         ((or "move" "copy")
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
         (_ (error "Unknown action: ~A" action))))

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
        (loop for
              p
              in
              effective-params
              for
              pname
              =
              (first p)
              for
              key-str
              =
              (string-downcase (substitute #\_ #\- (string pname)))
              for
              req-p
              =
              (getf (cdr p) :required)
              collect
              `
              (setf (gethash ,key-str ,props-var) (build-prop-schema ',p))
              when
              req-p
              collect
              `
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
             (loop for
                   p
                   in
                   effective-params
                   for
                   pname
                   =
                   (first p)
                   unless
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

(defun format-ancestor-spine-item
       (tree prefix next-idx)
  "Format a single ancestral spine level showing how NEXT-IDX was entered, eliding siblings."
  (let*
      ((ancestor (structural-editing-mcp.tree:resolve-tree-scope tree prefix))
       (tag (structural-editing-mcp.tree:get-node-tag ancestor))
       (children (structural-editing-mcp.tree:get-node-children ancestor))
       (total (length children)))
    (cond
      ((eq tag :workspace)
        (format nil
                "Spine [~{~A~^, ~}] :WORKSPACE (~D active dialects; entering dialect [~A])"
                prefix
                total
                next-idx))
      ((member tag structural-editing-mcp.workspace:*known-dialects*)
        (format nil
                "Spine [~{~A~^, ~}] ~A (~D files; entering file [~A])"
                prefix
                tag
                total
                next-idx))
      ((eq tag :file)
        (let
            ((filepath (structural-editing-mcp.workspace:get-filepath prefix)))
          (format nil
                  "Spine [~{~A~^, ~}] :FILE (~A) — ~D top-level forms; entering form [~A] (~D lateral siblings omitted)"
                  prefix
                  (or filepath "unknown")
                  total
                  next-idx
                  (max 0 (1- total)))))
      (t
        (let
            ((sig (extract-node-signature ancestor :max-elements 2 :max-length 50)))
          (format nil
                  "Spine [~{~A~^, ~}] ~A: ~A — ~D children; entering child [~A] (~D lateral siblings omitted)"
                  prefix
                  tag
                  sig
                  total
                  next-idx
                  (max 0 (1- total))))))))

(defun format-node-slice (tree path args)
  "Format a vertical spine from workspace root down to PATH, strictly eliding lateral siblings."
  (let*
      ((target-node (structural-editing-mcp.tree:resolve-tree-scope tree path))
       (depth (or (gethash "depth" args) 2))
       (limit (gethash "limit" args))
       (offset (or (gethash "offset" args) 0))
       (mode (or (gethash "mode" args) "full")))
    (if (null target-node)
      "Target node not found at given path."
      (with-output-to-string (s)
        (format s "=== Ancestral Spine (Vertical Ray down to ~A) ===~%" path)
        (loop for
              i
              from
              0
              below
              (length path)
              for
              prefix
              =
              (subseq path 0 i)
              for
              next-idx
              =
              (nth i path)
              for
              spine-line
              =
              (format-ancestor-spine-item tree prefix next-idx)
              do
              (format s "  ~A~%" spine-line))
        (format s "~%=== Target Node [~{~A~^, ~}] ===~%" path)
        (format s
                "~A"
                (format-node-preview target-node
                                     :depth
                                     depth
                                     :limit
                                     limit
                                     :offset
                                     offset
                                     :mode
                                     mode))))))

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

(defun format-workspaces-list (list)
  "Format list of workspace plists into readable string summary."
  (with-output-to-string (s)
    (format s "Workspaces (~A):~%" (length list))
    (dolist (w list)
      (format s
              "  - [~A] (parent: ~A, revision: ~A, files: ~A, dirty: ~A)~%"
              (getf w :id)
              (or (getf w :parent-id) "none")
              (getf w :revision)
              (getf w :file-count)
              (if (getf w :dirty-p)
                "YES"
                "no")))))

(defun format-workspace-status-summary (st)
  "Format workspace status plist ST into a readable summary string."
  (with-output-to-string (s)
    (format s "Workspace: ~A~%" (getf st :id))
    (format s
            "  Parent: ~A (base revision: ~A)~%"
            (or (getf st :parent-id) "none")
            (getf st :base-revision))
    (format s "  Current Revision: ~A~%" (getf st :revision))
    (let
        ((dirty (getf st :dirty-files))
         (clean (getf st :clean-files))
         (snaps (getf st :snapshots)))
      (format s "  Dirty Files (~A):~%~{    - ~A~%~}" (length dirty) dirty)
      (format s "  Clean Files (~A):~%~{    - ~A~%~}" (length clean) clean)
      (format s "  Snapshots (~A):~%~{    - ~A~%~}" (length snaps) snaps))))

(defun format-diff-section
       (s label items &optional prefix)
  "Format a list of ITEMS under LABEL to stream S with optional line PREFIX."
  (when items
    (if prefix
      (format s
              "  ~A (~A):~%~{    ~A ~A~%~}"
              label
              (length items)
              (loop for
                    x
                    in
                    items
                    collect
                    prefix
                    collect
                    x))
      (format s "  ~A (~A):~%~{    - ~A~%~}" label (length items) items))))

(defun format-file-diff-summary (s fdiff)
  "Format detailed per-file diff entry FDIFF to stream S."
  (let
      ((file (getf fdiff :file)) (status (getf fdiff :status)))
    (format s "    File: ~A (~A)~%" file status)
    (when (getf fdiff :added-forms)
      (format s "      Added forms: ~A~%" (getf fdiff :added-forms)))
    (when (getf fdiff :removed-forms)
      (format s "      Removed forms: ~A~%" (getf fdiff :removed-forms)))
    (when (getf fdiff :modified-forms)
      (format s "      Modified forms (~A):~%" (length (getf fdiff :modified-forms)))
      (dolist (m (getf fdiff :modified-forms))
        (format s
                "        - Form ~A [~{~A~^, ~}]: ~A~%"
                (getf m :index)
                (or (getf m :path) '())
                (ellipsize (or (getf m :new-snippet) (getf m :old-snippet) "") 40))))))

(defun format-workspace-diff-summary (df)
  "Format workspace diff plist DF into a readable summary string."
  (with-output-to-string (s)
    (format s
            "Diff between [~A] and [~A]:~%"
            (getf df :source-id)
            (getf df :target-id))
    (let
        ((so (getf df :source-only))
         (to (getf df :target-only))
         (smo (getf df :source-modified-only))
         (tmo (getf df :target-modified-only))
         (ast-merge (getf df :ast-mergeable))
         (both (getf df :modified-in-both))
         (details (getf df :conflict-details))
         (ast-diff (getf df :ast-differing-files)))
      (format-diff-section s "Files only in source" so "+")
      (format-diff-section s "Files only in target" to "-")
      (format-diff-section s "Source modified only" smo "*")
      (format-diff-section s "Target modified only" tmo "*")
      (format-diff-section s
                           "AST-mergeable disjoint files (safe to auto-merge)"
                           ast-merge)
      (when both
        (format s
                "  COLLIDING MODIFICATIONS in both (~A):~%~{    ! ~A~%~}"
                (length both)
                both)
        (when details
          (dolist (d details)
            (format s
                    "      File: ~A (conflicting form indices: ~{~A~^, ~})~%"
                    (getf d :file)
                    (getf d :conflicts)))))
      (format-diff-section s "AST differing files" ast-diff)
      (when
          (and (null so)
               (null to)
               (null smo)
               (null tmo)
               (null ast-merge)
               (null both)
               (null ast-diff))
        (format s "  No differences detected.~%")))))

(defun format-rebase-conflict-summary
       (ws-id conflicts)
  "Format a list of rebase collisions into an informative error message."
  (with-output-to-string (s)
    (format s
            "Rebase of workspace ~S encountered ~A collision(s):~%"
            ws-id
            (length conflicts))
    (dolist (c conflicts)
      (format s "  - File: ~A (modified in both)~%" (getf c :file)))))

;;; ----------------------------------------------------------------------

;;; Tool Definitions (Unified Declarations & Handlers)

;;; ----------------------------------------------------------------------

(define-mcp-tool "read_node"
                 (:description
                   "Inspect any node in the AST or workspace. Returns rendered code and a nested tree of child paths up to 'depth' levels. Supports 'mode': 'skeleton' for compact metadata stubs on wide trees. BEST PRACTICE: Always read parent forms (e.g. [0, 10]) to see the entire expression and its child paths at once—do NOT probe child indices one-by-one. Use 'read_slice' to cast a vertical ray down to a specific nested node without lateral context blowout. Pass 'load_files' on initial call to populate the workspace from disk."
                   :mutation
                   nil)
                 ((path :type :path)
                  (depth :type
                         :integer
                         :doc
                         "Recursion depth for displaying nested children and their paths (default: 2)."
                         :default
                         2)
                  (limit :type
                         :integer
                         :doc
                         "Optional maximum number of child nodes or files to display in preview (default: 50)."
                         :default
                         50)
                  (offset :type
                          :integer
                          :doc
                          "Optional 0-indexed child offset to start displaying from (default: 0)."
                          :default
                          0)
                  (mode :type
                        :string
                        :doc
                        "Display mode: 'full' (default), 'skeleton' (compact metadata stubs for wide trees), or 'auto' (automatic skeleton for massive nodes)."
                        :enum
                        ("full" "skeleton" "auto")
                        :default
                        "auto")
                  (load_files :type :load-files))
  (bt:with-lock-held
    (structural-editing-mcp.workspace:*workspace-lock*)
    (let ((files-to-load (to-list load_files)))
      (when files-to-load
        (structural-editing-mcp.workspace:load-into-workspace files-to-load)))
    (unless
        structural-editing-mcp.workspace:*workspace-tree*
      (structural-editing-mcp.workspace:init-workspace))
    (structural-editing-mcp.workspace:record-agent-read agent-id)
    (let
        ((node
           (structural-editing-mcp.tree:resolve-tree-scope
             structural-editing-mcp.workspace:*workspace-tree*
             path)))
      (format-node-preview node :depth depth :limit limit :offset offset :mode mode))))

(define-mcp-tool "read_slice"
                 (:description
                   "Inspect a vertical ray/spine path down to a specific target AST node. Renders the ancestral hierarchy (e.g. file, let, defun) leading to the target node while strictly eliding lateral siblings at each level, then hydrates the target node up to 'depth'. Ideal for inspecting deeply nested code in wide trees without context window blowout."
                   :mutation
                   nil)
                 ((path :type
                        :path
                        :doc
                        "Target AST path (e.g. [0, 0, 50, 2]) to cast the vertical ray down to."
                        :required
                        t)
                  (depth :type
                         :integer
                         :doc
                         "Recursion depth for displaying nested children of the target node (default: 2)."
                         :default
                         2)
                  (limit :type
                         :integer
                         :doc
                         "Optional maximum number of child nodes to display for the target node (default: 50)."
                         :default
                         50)
                  (offset :type
                          :integer
                          :doc
                          "Optional 0-indexed child offset for the target node."
                          :default
                          0)
                  (mode :type
                        :string
                        :doc
                        "Display mode for the target node: 'full' (default) or 'skeleton'."
                        :enum
                        ("full" "skeleton" "auto")
                        :default
                        "full")
                  (load_files :type :load-files))
  (bt:with-lock-held
    (structural-editing-mcp.workspace:*workspace-lock*)
    (let ((files-to-load (to-list load_files)))
      (when files-to-load
        (structural-editing-mcp.workspace:load-into-workspace files-to-load)))
    (unless
        structural-editing-mcp.workspace:*workspace-tree*
      (structural-editing-mcp.workspace:init-workspace))
    (structural-editing-mcp.workspace:record-agent-read agent-id)
    (format-node-slice structural-editing-mcp.workspace:*workspace-tree* path args)))

(define-mcp-tool "ast_modify"
                 (:description
                   "Mutate AST nodes in memory. Actions: 'insert' (adds new_node before path, or at child index if index is given), 'overwrite' (replaces node at path with new_node), 'wrap' (wraps node or child range with parens, brackets, or enclosing form). WARNING: Always target the exact, specific child path (e.g. [0, 1, 5, 2]) for your mutation. DO NOT attempt to overwrite a parent node using a truncated child list, as this will delete all un-rendered siblings. NOTE: Automatically returns an updated preview of the enclosing parent node; separate verification reads are unnecessary. To preserve safety in multi-agent workflows, fork a workspace first with 'workspace_manage'."
                   :mutation
                   t)
                 ((path :type
                        :path
                        :doc
                        "Target AST path (e.g. [0, 2] to target form 2 in file 0)."
                        :required
                        t)
                  (action :type
                          :string
                          :doc
                          "The modification action to perform."
                          :enum
                          ("insert" "overwrite" "wrap")
                          :required
                          t)
                  (new_node :type
                            :string
                            :doc
                            "For insert/overwrite: the S-expression code string (e.g. '(defun foo () 42)'). For wrap: delimiter keyword (':paren', ':square', ':curly') or enclosing form string (e.g. '(when condition)')."
                            :required
                            t)
                  (index :type
                         :integer
                         :doc
                         "Optional child index for insert. If omitted when inserting, uses the last element of path.")
                  (end_index :type
                             :integer
                             :doc
                             "Optional ending child index for range wrapping with action 'wrap'.")
                  (force :type
                         :boolean
                         :doc
                         "Optional safeguard bypass: set to true to force overwriting a node that has more than 50 children."))
  (let
      ((tree structural-editing-mcp.workspace:*workspace-tree*))
    (when (equal action "overwrite")
      (let*
          ((target-node (structural-editing-mcp.tree:get-node-at-path tree path))
           (child-count
             (length (structural-editing-mcp.tree:get-node-children target-node))))
        (when (and (> child-count 50) (not force))
          (error
            "Safeguard: Node at path ~A has ~D children. Overwriting a large parent node directly will delete all its children. To modify an element inside, target the specific child path instead (e.g. append child index to path). If you genuinely intend to overwrite the entire collection, pass 'force': true."
            path
            child-count))))
    (setf
      structural-editing-mcp.workspace:*workspace-tree*
      (match action
             ("insert" (perform-insert tree path new_node index))
             ("overwrite"
              (structural-editing-mcp.edit:overwrite-expression tree path new_node))
             ("wrap" (perform-wrap tree path new_node end_index index))
             (_ (error "Unknown action: ~A" action))))
    (format-mutation-result
      (fmt "Successfully executed ~A at ~A" action path)
      path
      structural-editing-mcp.workspace:*workspace-tree*)))

(define-mcp-tool "ast_remove"
                 (:description
                   "Remove, unwrap, or promote AST nodes in memory. Actions: 'delete' (deletes the node at path), 'unwrap' (removes enclosing collection, spilling children into parent), 'promote' (replaces parent node with the child node at path). WARNING: Always target the exact, specific child path for removal. NOTE: Automatically returns an updated preview of the parent form."
                   :mutation
                   t)
                 ((path :type
                        :path
                        :doc
                        "The AST path of the node to remove/unwrap/promote."
                        :required
                        t)
                  (action :type
                          :string
                          :doc
                          "The removal action to perform."
                          :enum
                          ("delete" "unwrap" "promote")
                          :required
                          t)
                  (new_node :type
                            :string
                            :doc
                            "Optional replacement node string (for action 'replace' or fallback)."))
  (let
      ((tree structural-editing-mcp.workspace:*workspace-tree*))
    (setf
      structural-editing-mcp.workspace:*workspace-tree*
      (match action
             ("delete" (structural-editing-mcp.edit:delete-node tree path))
             ("unwrap" (structural-editing-mcp.edit:unwrap-node tree path))
             ("promote" (structural-editing-mcp.edit:promote-node tree path))
             (_ (error "Unknown action: ~A" action))))
    (format-mutation-result
      (fmt "Successfully executed ~A at ~A" action path)
      path
      structural-editing-mcp.workspace:*workspace-tree*)))

(define-mcp-tool "ast_relocate"
                 (:description
                   "Reorder, copy, swap, merge, or split AST nodes and collections in memory. Actions: 'move' (relocates node from source_path to target_path or target_index), 'copy' (duplicates node from source_path into target_path), 'swap' (interchanges positions of two nodes at source_path and target_path), 'merge' (combines two collections or files into one), 'split' (divides a collection into two at split_index). NOTE: Automatically returns a preview of the enclosing form."
                   :mutation
                   t)
                 ((action :type
                          :string
                          :doc
                          "The relocation action to perform."
                          :enum
                          ("move" "copy" "swap" "merge" "split")
                          :required
                          t)
                  (source_path :type
                               :path
                               :doc
                               "Source AST path of the node to move, copy, swap, or merge.")
                  (target_path :type :path :doc "Target AST path for move, copy, swap, or merge.")
                  (target_index :type
                                :integer
                                :doc
                                "Optional child index within target_path to insert the relocated node.")
                  (index :type :integer :doc "Optional index within target.")
                  (split_index :type
                               :integer
                               :doc
                               "For action 'split': the 0-indexed child position at which to split the collection.")
                  (separator :type :string :doc "Optional separator string for merge."))
  (let*
      ((src (or (to-list source_path) path))
       (tgt (to-list target_path))
       (idx (or target_index index)))
    (setf
      structural-editing-mcp.workspace:*workspace-tree*
      (perform-relocate-action action
                               src
                               tgt
                               idx
                               structural-editing-mcp.workspace:*workspace-tree*))
    (format-mutation-result
      (fmt "Successfully executed ~A from ~A to ~A" action src tgt)
      tgt
      structural-editing-mcp.workspace:*workspace-tree*)))

(define-mcp-tool "ast_search"
                 (:description
                   "Search the workspace or a subtree for symbols, identifiers, function calls, or literal values. Fast AST-aware token searching that returns matched AST paths."
                   :mutation
                   nil)
                 ((query :type
                         :string
                         :doc
                         "Symbol or text to search for (case-insensitive substring/symbol match)."
                         :required
                         t)
                  (path :type
                        :path
                        :doc
                        "Optional AST path to constrain the search scope. If omitted, searches the entire workspace."))
  (let
      ((results
         (perform-search structural-editing-mcp.workspace:*workspace-tree* path query)))
    (if results
      (fmt "Found ~A matches. Paths:~%~{~A~^~%~}" (length results) results)
      (fmt "No matches found for '~A' at path ~A" query path))))

(define-mcp-tool "ast_rename"
                 (:description
                   "Rename all occurrences of an identifier/symbol across the workspace or within a specific subtree. Operates strictly on symbol leaf nodes, preserving comments and string literals."
                   :mutation
                   t)
                 ((old_name :type
                            :string
                            :doc
                            "The exact symbol/string to replace (e.g. 'make-api-call')."
                            :required
                            t)
                  (new_name :type
                            :string
                            :doc
                            "The new symbol/string to replace it with (e.g. 'execute-api-call')."
                            :required
                            t)
                  (path :type
                        :path
                        :doc
                        "Optional AST path to constrain the bulk rename to a specific subtree. If omitted, renames globally across the workspace."))
  (setf
    structural-editing-mcp.workspace:*workspace-tree*
    (perform-rename
      structural-editing-mcp.workspace:*workspace-tree*
      path
      old_name
      new_name))
  (fmt "Successfully renamed all occurrences of '~A' to '~A'." old_name new_name))

(define-mcp-tool "ast_replace_pattern"
                 (:description
                   "Search the workspace for a structural Lisp pattern and replace it with a new pattern, preserving matched variables (e.g. pattern='(foo ?x ?y)', replacement='(bar ?y ?x)'). Ideal for semantic API migrations and structural refactorings."
                   :mutation
                   t)
                 ((pattern :type
                           :string
                           :doc
                           "The pattern to match. Variables start with '?' (e.g. '(make-api-call ?method ?url ?headers ?body)')."
                           :required
                           t)
                  (replacement :type
                               :string
                               :doc
                               "The replacement template (e.g. '(make-api-call ?url ?method :headers ?headers :body ?body)')."
                               :required
                               t))
  (setf
    structural-editing-mcp.workspace:*workspace-tree*
    (structural-editing-mcp.refactor:replace-pattern
      structural-editing-mcp.workspace:*workspace-tree*
      pattern
      replacement))
  (fmt "Successfully executed pattern replacement across workspace."))

(define-mcp-tool "ast_extract_variable"
                 (:description
                   "Extracts an AST node into a local `let` binding wrapped around its immediate parent. Preserves sub-expression structure and automatically replaces the node with the bound variable."
                   :mutation
                   t)
                 ((path :type :path :doc "AST path of the node to extract." :required t)
                  (variable_name :type
                                 :string
                                 :doc
                                 "The name of the new variable to bind it to."
                                 :required
                                 t))
  (let
      ((eff-dialect (or dialect structural-editing-mcp.parser:*current-dialect*)))
    (setf
      structural-editing-mcp.workspace:*workspace-tree*
      (structural-editing-mcp.refactor:extract-variable
        structural-editing-mcp.workspace:*workspace-tree*
        path
        variable_name
        :dialect
        eff-dialect))
    (fmt "Successfully extracted node at ~A into variable '~A'." path variable_name)))

(define-mcp-tool "ast_extract_function"
                 (:description
                   "Extracts an AST node into a new top-level function definition and replaces the original node with a call to the new function. Emits the new function right before the current top-level form."
                   :mutation
                   t)
                 ((path :type
                        :path
                        :doc
                        "AST path of the node to extract into a function."
                        :required
                        t)
                  (function_name :type :string :doc "The name of the new function." :required t)
                  (params :type
                          :string-array
                          :doc
                          "Optional list of parameter names for the new function."))
  (let
      ((eff-dialect (or dialect structural-editing-mcp.parser:*current-dialect*)))
    (setf
      structural-editing-mcp.workspace:*workspace-tree*
      (structural-editing-mcp.refactor:extract-function
        structural-editing-mcp.workspace:*workspace-tree*
        path
        function_name
        :params
        (to-list params)
        :dialect
        eff-dialect))
    (fmt "Successfully extracted node at ~A into function '~A'." path function_name)))

(define-mcp-tool "ast_lint"
                 (:description
                   "Run static analysis and structural linting to identify code smells, anti-patterns, and opportunities for refactoring. Returns a list of findings with AST paths, messages, severity, and suggested quick-fixes."
                   :mutation
                   nil)
                 ((path :type
                        :path
                        :doc
                        "Optional AST path to lint a specific node or file. If omitted, lints the entire workspace.")
                  (dialect :type :dialect)
                  (rules :type
                         :string-array
                         :doc
                         "Optional list of rule names to run (e.g. ['if-progn-to-when', 'redundant-progn']). If omitted, runs all rules."))
  (run-tool-lint
    structural-editing-mcp.workspace:*workspace-tree*
    path
    dialect
    args))

(define-mcp-tool "ast_complexity_metrics"
                 (:description
                   "Calculate cyclomatic complexity and nesting depth metrics for functions and top-level forms. Identifies candidates for functional decomposition."
                   :mutation
                   nil)
                 ((path :type
                        :path
                        :doc
                        "Optional AST path to analyze. If omitted, analyzes all functions in the workspace.")
                  (dialect :type :dialect)
                  (min_complexity :type
                                  :integer
                                  :doc
                                  "Minimum cyclomatic complexity threshold to include in report (default: 1)."
                                  :default
                                  1)
                  (min_depth :type
                             :integer
                             :doc
                             "Minimum parenthetical nesting depth threshold to include in report (default: 1)."
                             :default
                             1))
  (run-tool-complexity
    structural-editing-mcp.workspace:*workspace-tree*
    path
    dialect
    args))

(define-mcp-tool "ast_find_duplicates"
                 (:description
                   "Find duplicate code structures and identical subtrees across files in the workspace. Useful for identifying candidate utility functions to extract."
                   :mutation
                   nil)
                 ((path :type
                        :path
                        :doc
                        "Optional AST path to search within. If omitted, searches across all loaded files.")
                  (min_size :type
                            :integer
                            :doc
                            "Minimum node count of duplicate subtree (default: 3)."
                            :default
                            3)
                  (min_occurrences :type
                                   :integer
                                   :doc
                                   "Minimum number of occurrences to report (default: 2)."
                                   :default
                                   2))
  (run-tool-duplicates
    structural-editing-mcp.workspace:*workspace-tree*
    path
    args))

(define-mcp-tool "ast_analyze_bindings"
                 (:description
                   "Analyze lexical variable bindings, scope chains, unused variables, and variable shadowing across the workspace or within a specific function/node."
                   :mutation
                   nil)
                 ((path :type
                        :path
                        :doc
                        "Optional AST path to analyze. If omitted, analyzes all files in the workspace.")
                  (dialect :type :dialect))
  (run-tool-bindings
    structural-editing-mcp.workspace:*workspace-tree*
    path
    dialect
    args))

(define-mcp-tool "ast_suggest_refactorings"
                 (:description
                   "Aggregate analysis findings from lint, complexity, duplicates, and binding analysis into a prioritized refactoring plan with specific AST paths and actions."
                   :mutation
                   nil)
                 ((path :type
                        :path
                        :doc
                        "Optional AST path to analyze. If omitted, analyzes the whole workspace.")
                  (dialect :type :dialect)
                  (min_priority :type
                                :string
                                :doc
                                "Minimum priority threshold to include in plan: 'critical', 'warning', 'suggestion', 'style' (default: 'style')."
                                :enum
                                ("critical" "warning" "suggestion" "style")
                                :default
                                "style"))
  (run-tool-suggestions
    structural-editing-mcp.workspace:*workspace-tree*
    path
    dialect
    args))

(define-mcp-tool "workspace_create_file"
                 (:description
                   "Create a new source file in the staged workspace without writing to disk. The file is registered in memory, staged for AST operations, and persisted only when commit_workspace is called."
                   :mutation
                   nil)
                 ((path :type
                        :string
                        :doc
                        "File system path of the new file (e.g. '/path/to/src/new-module.lisp').")
                  (filepath :type :string :doc "File system path of the new file.")
                  (file_path :type :string :doc "File system path of the new file.")
                  (content :type
                           :string
                           :doc
                           "Initial file contents (default: empty file)."
                           :default
                           "")
                  (dialect :type :dialect))
  (let*
      ((target-ws (or workspace_id "default"))
       (file-path (or filepath file_path path))
       (ws (structural-editing-mcp.workspace:get-workspace target-ws)))
    (unless file-path (error "filepath is required for workspace_create_file"))
    (structural-editing-mcp.workspace:add-file-to-workspace
      file-path
      :content
      (or content "")
      :dialect
      dialect
      :ctx
      ws)
    (fmt
      "File ~S created successfully in workspace ~S (staged in memory; uncommitted)."
      file-path
      target-ws)))

(define-mcp-tool "workspace_rebase"
                 (:description
                   "Rebase a branch workspace onto its parent workspace, integrating upstream changes. Resolves non-colliding changes automatically and detects collisions."
                   :mutation
                   nil)
                 ((source_workspace_id :type
                                       :string
                                       :doc
                                       "The branch workspace to rebase (e.g. 'agent-1').")
                  (target_workspace_id :type
                                       :string
                                       :doc
                                       "Target workspace to rebase onto (defaults to 'default').")
                  (onto_workspace_id :type
                                     :string
                                     :doc
                                     "Target workspace to rebase onto (defaults to parent).")
                  (strategy :type
                            :string
                            :doc
                            "Rebase strategy: 'three-way' (default), 'theirs' (prefer upstream on collision), 'ours' (keep branch changes on collision)."
                            :enum
                            ("three-way" "theirs" "ours" "error")
                            :default
                            "three-way"))
  (let*
      ((ws-id (or source_workspace_id workspace_id "default"))
       (onto-id (or target_workspace_id onto_workspace_id "default"))
       (strat (intern (string-upcase (or strategy "three-way")) :keyword))
       (res
         (structural-editing-mcp.workspace:rebase-workspace
           ws-id
           :onto-id
           onto-id
           :strategy
           strat)))
    (if (getf res :conflicts)
      (format-rebase-conflict-summary ws-id (getf res :conflicts))
      (fmt
        "Workspace ~S successfully rebased onto ~S (~A file(s) updated)."
        ws-id
        onto-id
        (length (getf res :updated-files))))))

(define-mcp-tool "workspace_manage"
                 (:description
                   "Manage workspace lifecycle: 'list' (all workspaces and revisions), 'create' (new empty workspace), 'delete' (remove workspace), 'clear' (reset workspace), 'fork' (create isolated branch workspace), 'snapshot' (checkpoint workspace revision), 'restore' (rollback to checkpoint), 'create_file' (add empty file in memory), 'rebase' (rebase branch onto parent), 'reload' (reload disk files)."
                   :mutation
                   nil)
                 ((action :type
                          :string
                          :doc
                          "Lifecycle action."
                          :enum
                          ("list" "create"
                           "delete"
                           "clear"
                           "fork"
                           "snapshot"
                           "restore"
                           "create_file"
                           "add_file"
                           "rebase"
                           "reload")
                          :default
                          "list")
                  (target_id :type
                             :string
                             :doc
                             "Target workspace identifier for create, fork, or rebase.")
                  (source_id :type
                             :string
                             :doc
                             "Source workspace identifier for fork (default: 'default')."
                             :default
                             "default")
                  (snapshot_name :type
                                 :string
                                 :doc
                                 "Name for snapshot or restore checkpoint."
                                 :default
                                 "checkpoint")
                  (force :type
                         :boolean
                         :doc
                         "Optional bypass flag for clearing modified workspaces or reloading.")
                  (files :type
                         :string-array
                         :doc
                         "Optional file list for reload or selective restore."))
  (let*
      ((act (string-downcase (or action "list")))
       (ws-id (or workspace_id "default"))
       (src (or source_id "default"))
       (tgt (or target_id workspace_id))
       (snap (or snapshot_name "checkpoint"))
       (file-list (to-list files)))
    (match act
           ("list"
            (format-workspaces-list (structural-editing-mcp.workspace:list-workspaces)))
           ("create"
            (unless tgt (error "target_id or workspace_id required for create"))
            (structural-editing-mcp.workspace:create-workspace tgt)
            (fmt "Workspace ~S created successfully." tgt))
           ("delete"
            (structural-editing-mcp.workspace:delete-workspace ws-id)
            (fmt "Workspace ~S deleted successfully." ws-id))
           ("clear"
            (let
                ((ws (structural-editing-mcp.workspace:get-workspace ws-id)))
              (structural-editing-mcp.workspace:clear-workspace ws :force force)
              (fmt "Workspace ~S cleared successfully." ws-id)))
           ("fork"
            (unless tgt (error "target_id required for fork"))
            (structural-editing-mcp.workspace:fork-workspace src tgt)
            (fmt "Workspace ~S successfully forked into ~S." src tgt))
           ((or "snapshot" "restore") (manage-snapshot-restore act ws-id snap))
           ((or "create_file" "add_file") (manage-create-file args ws-id))
           ("rebase" (manage-rebase-workspace args ws-id))
           ("reload" (manage-reload-workspace ws-id file-list force))
           (_ (error "Unknown workspace_manage action: ~A" act)))))

(define-mcp-tool "workspace_status"
                 (:description
                   "Query detailed status of a workspace: dirty/clean files, current/base revisions, snapshots."
                   :mutation
                   nil)
                 ()
  (let*
      ((ws-id (or workspace_id "default"))
       (ws (structural-editing-mcp.workspace:get-workspace ws-id))
       (st (structural-editing-mcp.workspace:workspace-status ws)))
    (format-workspace-status-summary st)))

(define-mcp-tool "workspace_diff"
                 (:description
                   "Compute AST and file-level difference between two workspaces or against base revision. Highlights disjoint/auto-mergeable files and colliding modifications."
                   :mutation
                   nil)
                 ((source_workspace_id :type :string :doc "Source workspace identifier.")
                  (target_workspace_id :type
                                       :string
                                       :doc
                                       "Target workspace identifier (default: 'default').")
                  (other_workspace_id :type
                                      :string
                                      :doc
                                      "The workspace ID to compare against (e.g. 'default' or a feature branch)."))
  (let*
      ((ws-a (or source_workspace_id workspace_id "default"))
       (ws-b (or target_workspace_id other_workspace_id "default"))
       (diff-plist (structural-editing-mcp.workspace:diff-workspaces ws-a ws-b)))
    (format-workspace-diff-summary diff-plist)))

(define-mcp-tool "workspace_merge"
                 (:description
                   "Merge changes from another workspace into the current workspace with AST-level disjoint merge. Safely combines disjoint top-level forms within files without text conflicts."
                   :mutation
                   nil)
                 ((source_workspace_id :type
                                       :string
                                       :doc
                                       "Source workspace identifier to merge from."
                                       :required
                                       t)
                  (target_workspace_id :type
                                       :string
                                       :doc
                                       "Target workspace identifier receiving changes (default: 'default')."
                                       :default
                                       "default")
                  (files :type
                         :string-array
                         :doc
                         "Optional list of specific files to transfer instead of merging all modified files."))
  (let*
      ((src-id source_workspace_id)
       (tgt-id (or target_workspace_id workspace_id "default"))
       (file-list (to-list files))
       (res
         (structural-editing-mcp.workspace:merge-workspaces
           src-id
           tgt-id
           :files
           file-list)))
    (fmt
      "Successfully merged workspace ~S into ~S (~A merge, ~A file(s) transferred)."
      src-id
      tgt-id
      (getf res :action)
      (length (getf res :merged-files)))))

(define-mcp-tool "commit_workspace"
                 (:description
                   "Persist in-memory workspace modifications to disk files. ONLY modified/dirty files are written. Clean files and unmodified forms are preserved bit-for-bit."
                   :mutation
                   nil)
                 ((files :type
                         :string-array
                         :doc
                         "Optional list of file paths to persist. If omitted, persists all dirty files in the workspace."))
  (let ((file-list (to-list files)))
    (structural-editing-mcp.workspace:write-workspace file-list)
    (if file-list
      (fmt "Committed ~A specified file(s) to disk successfully." (length file-list))
      (fmt
        "Workspace committed to disk successfully.~%TIP: Remember to run bash test/verification commands to confirm your changes compile and pass tests!"))))

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
               "errorCode"
               error-code
               "errorType"
               error-type
               "isError"
               t)))
    (when extra-fields
      (loop for
            (k v)
            on
            extra-fields
            by
            #'cddr
            do
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

(defun handle-message (msg)
  "Dispatch a parsed JSON-RPC message."
  (let
      ((jsonrpc (href msg "jsonrpc"))
       (id (href msg "id"))
       (method (href msg "method"))
       (params (href msg "params")))
    (when (equal jsonrpc "2.0")
      (match method
             ("initialize" (handle-initialize id params))
             ("notifications/initialized" nil)
             ("tools/list" (handle-tools-list id params))
             ("tools/call" (handle-tools-call id params))
             (_ (when id (send-error id -32601 (fmt "Method not found: ~A" method))))))))

(defun start-server ()
  "Start the MCP server loop over stdin/stdout with worker thread pool."
  (structural-editing-mcp.workspace:init-workspace)
  (start-worker-pool)
  (unwind-protect
      (let ((yason:*parse-json-arrays-as-vectors* nil))
        (loop
          (let
              ((line (read-line *standard-input* nil :eof)))
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