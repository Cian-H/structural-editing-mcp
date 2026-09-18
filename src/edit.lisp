(defpackage :structural-editing-mcp.edit
  (:use
    :cl
    :trivia
    :alexandria
    :structural-editing-mcp.parser
    :structural-editing-mcp.tree
    :structural-editing-mcp.utils
    :structural-editing-mcp.conditions)
  (:import-from :serapeum :take :drop :halves)
  (:export
    :insert-node
    :insert-expression
    :delete-node
    :overwrite-node
    :overwrite-expression
    :copy-node
    :pop-node
    :move-node
    :swap-nodes
    :wrap-node
    :wrap-range
    :unwrap-node
    :promote-node
    :split-node
    :merge-nodes)
  (:documentation "Functional tree surgery primitives for structural editing."))

(in-package :structural-editing-mcp.edit)

(declaim (optimize (speed 2) (safety 3)))

(defun insert-node (tree parent-path index node)
  "Insert AST NODE into the children of the node at PARENT-PATH at the given INDEX."
  (update-node-at-path
    tree
    parent-path
    (lambda (parent)
      (match parent
             ((node p tag children)
              `(:path ,p ,tag ,@(take index children) ,node ,@(drop index children)))
             (_ parent)))))

(defun insert-expression (tree parent-path index source-string)
  "Parse SOURCE-STRING and insert its first expression into the children of the node at PARENT-PATH at the given INDEX."
  (let ((children (get-node-children (string-to-sexp source-string))))
    (if (null children)
      (error 'sexp-parse-error
             :token source-string
             :message "No valid expression found in source string to insert")
      (insert-node tree parent-path index (first children)))))

(defun delete-node (tree path)
  "Delete the node at PATH."
  (nth-value 0 (pop-node tree path)))

(defun overwrite-node (tree path node)
  "Replace the node at PATH with NODE."
  (update-node-at-path tree path (constantly node)))

(defun overwrite-expression (tree path source-string)
  "Parse SOURCE-STRING and replace the node at PATH with the first resulting expression."
  (let ((children (get-node-children (string-to-sexp source-string))))
    (if (null children)
      (error 'sexp-parse-error
             :token source-string
             :message "No valid expression found in source string to overwrite")
      (overwrite-node tree path (first children)))))

(defun copy-node (tree source-path target-parent-path target-index)
  "Copy the node at SOURCE-PATH and insert it at TARGET-INDEX under TARGET-PARENT-PATH."
  (let ((node (get-node-at-path tree source-path)))
    (unless node
      (error 'invalid-path-error
             :path source-path
             :tree tree
             :message (format nil "Source node at path ~A not found for copy" source-path)))
    (insert-node tree target-parent-path target-index node)))

(defun pop-node (tree target-path)
  "Remove the node at TARGET-PATH from the tree and return both the new tree and the removed node."
  (if (null target-path)
    (values nil tree)
    (let* ((idx (lastcar target-path))
           popped-node
           (new-tree
             (update-node-at-path
               tree
               (butlast target-path)
               (lambda (parent)
                 (match parent
                        ((node p tag children)
                         (setf popped-node (nth idx children))
                         `(:path ,p ,tag ,@(take idx children) ,@(drop (1+ idx) children)))
                        (_ parent))))))
      (values new-tree popped-node))))

(defun move-node (tree source-path target-parent-path target-index)
  "Move the node at SOURCE-PATH to TARGET-INDEX under TARGET-PARENT-PATH."
  (multiple-value-bind (new-tree node) (pop-node tree source-path)
    (unless node
      (error 'invalid-path-error
             :path source-path
             :tree tree
             :message (format nil "Source node at path ~A not found for move" source-path)))
    (insert-node new-tree target-parent-path target-index node)))

(defun swap-nodes (tree path1 path2)
  "Swap the nodes at PATH1 and PATH2."
  (let
      ((parent1 (butlast path1)) (parent2 (butlast path2)))
    (if
        (equal parent1 parent2)
      (let
          ((idx1 (lastcar path1)) (idx2 (lastcar path2)))
        (update-node-at-path
          tree
          parent1
          (lambda
              (parent)
            (match
              parent
              ((node p tag children)
               (let
                   ((child1 (nth idx1 children)) (child2 (nth idx2 children)))
                 `
                 (:path
                   ,p
                   ,tag
                   ,@
                   (loop
                     for
                     c
                     in
                     children
                     for
                     i
                     from
                     0
                     collect
                     (cond ((= i idx1) child2) ((= i idx2) child1) (t c))))))
              (_ parent)))))
      (let
          ((node1 (get-node-at-path tree path1)) (node2 (get-node-at-path tree path2)))
        (overwrite-node (overwrite-node tree path1 node2) path2 node1)))))

(defun wrap-node (tree path tag)
  "Wrap the node at PATH in a new collection node with TAG (e.g. :paren)."
  (update-node-at-path tree path (lambda (node) (list :path path tag node))))

(defun wrap-range (tree parent-path start-index end-index tag)
  "Wrap the children of PARENT-PATH from START-INDEX to END-INDEX in a new collection with TAG."
  (update-node-at-path
    tree
    parent-path
    (lambda
        (parent)
      (match
        parent
        ((node p ptag children)
         (let
             ((before (take start-index children))
              (slice (take (1+ (- end-index start-index)) (drop start-index children)))
              (after (drop (1+ end-index) children)))
           `
           (:path ,p ,ptag ,@before (:path ,p ,tag ,@slice) ,@after)))
        (_ parent)))))

(defun unwrap-node (tree path)
  "Unwrap the collection node at PATH, spilling its children into its parent."
  (if
      (null path)
    tree
    (let
        ((idx (lastcar path)))
      (update-node-at-path
        tree
        (butlast path)
        (lambda
            (parent)
          (match
            parent
            ((node p ptag children)
             (match
               (nth idx children)
               ((node _ _ inner-children)
                `
                (:path
                  ,p
                  ,ptag
                  ,@
                  (take idx children)
                  ,@inner-children
                  ,@
                  (drop (1+ idx) children)))
               (_ parent)))
            (_ parent)))))))

(defun promote-node (tree path)
  "Promote the node at PATH to replace its parent node."
  (if
      (null path)
    tree
    (let
        ((idx (lastcar path)))
      (update-node-at-path
        tree
        (butlast path)
        (lambda
            (parent)
          (match parent ((node _ _ children) (nth idx children)) (_ parent)))))))

(defun split-node (tree path child-index)
  "Split the collection node at PATH into two siblings at CHILD-INDEX."
  (update-node-at-path
    tree
    path
    (lambda
        (node)
      (match
        node
        ((node p tag children)
         (multiple-value-bind
             (left right)
             (halves children child-index)
           `
           (:path ,p ,tag (:path ,p ,tag ,@left) (:path ,p ,tag ,@right))))
        (_ node)))))

(defun merge-nodes (tree path1 path2)
  "Merge two sibling collection nodes at PATH1 and PATH2 into a single node."
  (let
      ((parent1 (butlast path1)) (parent2 (butlast path2)))
    (unless
        (equal parent1 parent2)
      (error
        'invalid-path-error
        :path
        (list path1 path2)
        :tree
        tree
        :message
        "Cannot merge nodes with different parents"))
    (let
        ((left-idx (min (lastcar path1) (lastcar path2)))
         (right-idx (max (lastcar path1) (lastcar path2))))
      (update-node-at-path
        tree
        parent1
        (lambda
            (parent)
          (match
            parent
            ((node p ptag children)
             (match
               (list (nth left-idx children) (nth right-idx children))
               ((list (node p1 tag1 ch1) (node _ _ ch2))
                `
                (:path
                  ,p
                  ,ptag
                  ,@
                  (take left-idx children)
                  (:path ,p1 ,tag1 ,@ch1 ,@ch2)
                  ,@
                  (take (- right-idx (1+ left-idx)) (drop (1+ left-idx) children))
                  ,@
                  (drop (1+ right-idx) children)))
               (_ parent)))
            (_ parent)))))))