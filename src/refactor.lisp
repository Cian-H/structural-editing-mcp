(defpackage :structural-editing-mcp.refactor
    (:use
        :cl
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.edit
        :alexandria)
  (:export :replace-pattern :extract-variable :extract-function))

(in-package :structural-editing-mcp.refactor)

(declaim (optimize (speed 2) (safety 3)))

;;; --- Pattern Matching ---


(defun variable-node-p (node)
  "Check if a leaf node is a variable (symbol starting with ?)."
  (multiple-value-bind
    (path tag val)
    (parse-node node)
    (declare (ignore path))
    (and (eq tag :leaf)
         (symbolp val)
         (plusp (length (symbol-name val)))
         (char= (char (symbol-name val) 0) #\?))))

(defun match-pattern (pattern target bindings)
  "Match TARGET node against PATTERN node. Return (values success new-bindings)."
  (cond
    ((variable-node-p pattern)
     (let*
        ((var-name (nth-value 2 (parse-node pattern)))
         (existing (assoc var-name bindings)))
        (if
            existing
            (if
              (string= (sexp-to-string target) (sexp-to-string (cdr existing)))
              (values t bindings)
              (values nil bindings))
            (values t (cons (cons var-name target) bindings)))))
    ((and (eq (get-node-tag pattern) :leaf) (eq (get-node-tag target) :leaf))
     (let
        ((pval (nth-value 2 (parse-node pattern)))
         (tval (nth-value 2 (parse-node target))))
        (if (equal pval tval) (values t bindings) (values nil bindings))))
    ((and
           (member (get-node-tag pattern) ' (:paren :square :curly))
           (eq (get-node-tag pattern) (get-node-tag target)))
     (let
        ((pchildren (get-node-children pattern)) (tchildren (get-node-children target)))
        (if
            (= (length pchildren) (length tchildren))
            (loop
                for
                p
                in
                pchildren
                for
                t-child
                in
                tchildren
                do
                (multiple-value-bind
              (success new-bindings)
              (match-pattern p t-child bindings)
              (if success (setf bindings new-bindings) (return (values nil bindings))))
                finally
                (return (values t bindings)))
            (values nil bindings))))
    (t (values nil bindings))))

(defun instantiate-pattern (pattern bindings)
  "Create a new AST node by substituting variables in PATTERN using BINDINGS."
  (cond
    ((variable-node-p pattern)
     (let*
        ((var-name (nth-value 2 (parse-node pattern)))
         (bound (cdr (assoc var-name bindings))))
        (if bound bound pattern)))
    ((member (get-node-tag pattern) ' (:leaf :comment)) pattern)
    (t
       (let
        ((children (get-node-children pattern)))
        (list*
               :path
               (get-node-path pattern)
               (get-node-tag pattern)
               (mapcar (lambda (c) (instantiate-pattern c bindings)) children))))))

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

;;; --- Variable Extraction ---


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
      (lambda
              (parent)
              (multiple-value-bind
          (p-path p-tag p-children)
          (parse-node parent)
          (declare (ignore p-path))
          (let
            ((new-children
                            (loop
                      for
                      child
                      in
                      p-children
                      for
                      i
                      from
                      0
                      collect
                      (if
                      (= i child-idx)
                      (list :path nil :leaf (intern (string-upcase var-name)))
                      child))))
            (let
              ((new-parent ` (:path nil ,p-tag ,@new-children)))
              (let
                ((let-ast (string-to-sexp (format nil "(let ((~A )))" var-name))))
                (let
                  ((let-node (first (get-node-children let-ast))))
                  (let*
                    ((bindings-list (second (get-node-children let-node)))
                     (first-binding (first (get-node-children bindings-list)))
                     (completed-binding
                        `
                        (:path nil :paren ,@ (get-node-children first-binding) ,target-node))
                     (completed-bindings-list ` (:path nil :paren ,completed-binding)))
                    `
                    (:path nil :paren (:path nil :leaf let) ,completed-bindings-list ,new-parent)))))))))))

;;; --- Function Extraction ---

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