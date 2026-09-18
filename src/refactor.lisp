(defpackage :structural-editing-mcp.refactor
  (:use :cl
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.edit
        :structural-editing-mcp.analysis
        :alexandria)
  (:export :replace-pattern
           :extract-variable
           :extract-function
           :match-pattern
           :instantiate-pattern))

(in-package :structural-editing-mcp.refactor)

(declaim (optimize (speed 2) (safety 3)))

(defun replace-pattern (tree pattern-str replacement-str)
  "Recursively search TREE, replacing subtrees that match PATTERN-STR with REPLACEMENT-STR."
  (let*
      ((pat-ast (first (get-node-children (string-to-sexp pattern-str))))
       (rep-ast (first (get-node-children (string-to-sexp replacement-str)))))
    (labels
        ((walk
             (node)
           (multiple-value-bind
               (match-p bindings)
               (match-pattern pat-ast node nil)
             (if
                 match-p
               (instantiate-pattern rep-ast bindings)
               (if
                   (member (get-node-tag node) ' (:leaf :comment))
                 node
                 (let
                     ((children (get-node-children node)))
                   (if
                       children
                     (list* :path (get-node-path node) (get-node-tag node) (mapcar #'walk children))
                     node)))))))
      (reindex-paths (walk tree)))))

(defun extract-variable (tree target-path var-name)
  "Extract the node at TARGET-PATH into a let binding around its parent."
  (when
      (or (null target-path) (null (cdr target-path)))
    (error "Cannot extract a top-level form."))
  (let*
      ((target-node (get-node-at-path tree target-path))
       (parent-path (butlast target-path))
       (child-idx (lastcar target-path)))
    (update-node-at-path
      tree
      parent-path
      (lambda (parent)
        (multiple-value-bind (p-path p-tag p-children)
                             (parse-node parent)
          (declare (ignore p-path))
          (let* ((new-children
                   (loop for child in p-children
                         for i from 0
                         collect (if (= i child-idx)
                                   (list :path nil :leaf (intern (string-upcase var-name)))
                                   child)))
                 (new-parent `(:path nil ,p-tag ,@new-children))
                 (let-ast (string-to-sexp (format nil "(let ((~A )))" var-name)))
                 (let-node (first (get-node-children let-ast)))
                 (bindings-list (second (get-node-children let-node)))
                 (first-binding (first (get-node-children bindings-list)))
                 (completed-binding
                   `(:path nil :paren ,@(get-node-children first-binding) ,target-node))
                 (completed-bindings-list `(:path nil :paren ,completed-binding)))
            `(:path nil :paren (:path nil :leaf let) ,completed-bindings-list ,new-parent)))))))

(defun find-file-path-and-top-index (tree target-path)
  "Find the file node path and top-level form index in that file for TARGET-PATH."
  (cond
    ((and (>= (length target-path) 3)
          (eq (get-node-tag tree) :workspace))
      (values (subseq target-path 0 2) (nth 2 target-path)))
    ((eq (get-node-tag tree) :file)
      (values '() (first target-path)))
    (t
      (values (butlast target-path) (lastcar target-path)))))

(defun extract-function (tree target-path function-name &key params)
  "Extract the node at TARGET-PATH into a new top-level function definition named FUNCTION-NAME."
  (when (null target-path)
    (error "Cannot extract root workspace/file node."))
  (let ((target-node (get-node-at-path tree target-path)))
    (unless target-node
      (error "Target node not found at path ~A" target-path))
    (multiple-value-bind (file-path top-idx) (find-file-path-and-top-index tree target-path)
      (let* ((param-list (mapcar (lambda (p) (if (symbolp p) (symbol-name p) p)) (ensure-list params)))
             (call-str (if param-list
                         (format nil "(~A ~{~A~^ ~})" function-name param-list)
                         (format nil "(~A)" function-name)))
             (call-ast (first (get-node-children (string-to-sexp call-str))))
             (def-str (format nil "(defun ~A (~{~A~^ ~}))" function-name param-list))
             (def-ast-base (first (get-node-children (string-to-sexp def-str))))
             (def-ast `(:path nil :paren ,@(get-node-children def-ast-base) ,target-node))
             (tree-with-call (overwrite-node tree target-path call-ast))
             (final-tree (insert-node tree-with-call file-path top-idx def-ast)))
        (reindex-paths final-tree)))))