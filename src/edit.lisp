(defpackage :structural-editing-mcp.edit
  (:use :cl
        :trivia
        :alexandria
        :structural-editing-mcp.parser
        :structural-editing-mcp.tree
        :structural-editing-mcp.utils)
  (:export :insert-node
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
           :merge-nodes))

(in-package :structural-editing-mcp.edit)

(defun insert-node (tree parent-path index node)
  "Insert AST NODE into the children of the node at PARENT-PATH at the given INDEX."
  (update-node-at-path tree parent-path
    (lambda (parent)
      (match parent
        ((list* :path p tag children)
         (list* :path p tag (insert-at children index node)))
        (_ parent)))))

(defun insert-expression (tree parent-path index source-string)
  "Parse SOURCE-STRING and insert its first expression into the children of the node at PARENT-PATH at the given INDEX."
  (when-let ((children (get-node-children (string-to-sexp source-string))))
    (insert-node tree parent-path index (first children))))

(defun delete-node (tree path)
  "Delete the node at PATH."
  (if (null path)
      nil
      (update-node-at-path tree (butlast path)
        (lambda (parent)
          (match parent
            ((list* :path p tag children)
             (list* :path p tag (remove-at children (lastcar path))))
            (_ parent))))))

(defun overwrite-node (tree path node)
  "Replace the node at PATH with NODE."
  (update-node-at-path tree path (constantly node)))

(defun overwrite-expression (tree path source-string)
  "Parse SOURCE-STRING and replace the node at PATH with the first resulting expression."
  (when-let ((children (get-node-children (string-to-sexp source-string))))
    (overwrite-node tree path (first children))))

(defun copy-node (tree source-path target-parent-path target-index)
  "Copy the node at SOURCE-PATH and insert it at TARGET-INDEX under TARGET-PARENT-PATH."
  (insert-node
    tree
    target-parent-path
    target-index
    (get-node-at-path tree source-path)))

(defun pop-node (tree target-path)
  "Remove the node at TARGET-PATH from the tree and return both the new tree and the removed node."
  (if (null target-path)
      (values nil tree)
      (let (popped-node)
        (let ((new-tree (update-node-at-path tree (butlast target-path)
                          (lambda (parent)
                            (match parent
                              ((list* :path p tag children)
                               (let ((idx (lastcar target-path)))
                                 (setf popped-node (nth idx children))
                                 (list* :path p tag (remove-at children idx))))
                              (_ parent))))))
          (values new-tree popped-node)))))

(defun move-node (tree source-path target-parent-path target-index)
  "Move the node at SOURCE-PATH to TARGET-INDEX under TARGET-PARENT-PATH."
  (multiple-value-bind (new-tree node) (pop-node tree source-path)
    (insert-node new-tree target-parent-path target-index node)))

(defun swap-nodes (tree path1 path2)
  "Swap the nodes at PATH1 and PATH2."
  (let ((parent1 (butlast path1))
        (parent2 (butlast path2)))
    (if (equal parent1 parent2)
        (let ((idx1 (lastcar path1))
              (idx2 (lastcar path2)))
          (update-node-at-path tree parent1
            (lambda (parent)
              (match parent
                ((list* :path p tag children)
                 (let ((child1 (nth idx1 children))
                       (child2 (nth idx2 children)))
                   (list* :path p tag
                          (loop for c in children
                                for i from 0
                                collect (cond ((= i idx1) child2)
                                              ((= i idx2) child1)
                                              (t c))))))
                (_ parent)))))
        (let ((node1 (get-node-at-path tree path1))
              (node2 (get-node-at-path tree path2)))
          (overwrite-node (overwrite-node tree path1 node2) path2 node1)))))

(defun wrap-node (tree path tag)
  "Wrap the node at PATH in a new collection node with TAG (e.g. :paren)."
  (update-node-at-path tree path
    (lambda (node)
      (list :path path tag node))))

(defun wrap-range (tree parent-path start-index end-index tag)
  "Wrap the children of PARENT-PATH from START-INDEX to END-INDEX in a new collection with TAG."
  (update-node-at-path tree parent-path
    (lambda (parent)
      (match parent
        ((list* :path p ptag children)
         (let ((before (subseq children 0 start-index))
               (slice (subseq children start-index (1+ end-index)))
               (after (nthcdr (1+ end-index) children)))
           `(:path ,p ,ptag
             ,@before
             (:path ,p ,tag ,@slice)
             ,@after)))
        (_ parent)))))

(defun unwrap-node (tree path)
  "Unwrap the collection node at PATH, spilling its children into its parent."
  (if (null path)
      tree
      (let ((idx (lastcar path)))
        (update-node-at-path tree (butlast path)
          (lambda (parent)
            (match parent
              ((list* :path p ptag children)
               (let ((before (subseq children 0 idx))
                     (inner-children (get-node-children (nth idx children)))
                     (after (nthcdr (1+ idx) children)))
                 `(:path ,p ,ptag
                   ,@before
                   ,@inner-children
                   ,@after)))
              (_ parent)))))))

(defun promote-node (tree path)
  "Promote the node at PATH to replace its parent node."
  (if (null path)
      tree
      (update-node-at-path tree (butlast path)
        (lambda (parent)
          (match parent
            ((list* :path _ _ children)
             (nth (lastcar path) children))
            (_ parent))))))

(defun split-node (tree path child-index)
  "Split the collection node at PATH into two siblings at CHILD-INDEX."
  (update-node-at-path tree path
    (lambda (node)
      (match node
        ((list* :path p tag children)
         (multiple-value-bind (left right) (split-at child-index children)
           `(:path ,p ,tag
             (:path ,p ,tag ,@left)
             (:path ,p ,tag ,@right))))
        (_ node)))))

(defun merge-nodes (tree path1 path2)
  "Merge two sibling collection nodes at PATH1 and PATH2 into a single node."
  (let ((parent1 (butlast path1))
        (parent2 (butlast path2)))
    (unless (equal parent1 parent2)
      (error "Cannot merge nodes with different parents: ~A and ~A" path1 path2))
    (let ((left-idx (min (lastcar path1) (lastcar path2)))
          (right-idx (max (lastcar path1) (lastcar path2))))
      (update-node-at-path tree parent1
        (lambda (parent)
          (match parent
            ((list* :path p ptag children)
             (match (list (nth left-idx children) (nth right-idx children))
               ((list (list* :path p1 tag1 ch1)
                      (list* :path _ _ ch2))
                `(:path ,p ,ptag
                  ,@(subseq children 0 left-idx)
                  (:path ,p1 ,tag1 ,@ch1 ,@ch2)
                  ,@(subseq children (1+ left-idx) right-idx)
                  ,@(nthcdr (1+ right-idx) children)))
               (_ parent)))
            (_ parent)))))))
