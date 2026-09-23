(defpackage :structural-editing-mcp.edit
  (:use
    :cl
    :trivia
    :alexandria
    :structural-editing-mcp.parser
    :structural-editing-mcp.tree
    :structural-editing-mcp.conditions)
  (:import-from :serapeum :take :drop :fmt)
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
             :message (fmt "Source node at path ~A not found for copy" source-path)))
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
                           (unless (and (>= idx 0) (< idx (length children)))
                             (error 'invalid-path-error
                                    :path target-path
                                    :tree tree
                                    :message (fmt "Child index ~D out of bounds for node at path ~A" idx p)))
                           (setf popped-node (nth idx children))
                           `(:path ,p ,tag ,@(take idx children) ,@(drop (1+ idx) children)))
                          (_ parent))))))
        (values new-tree popped-node))))

(defun move-node (tree source-path target-parent-path target-index)
  "Move the node at SOURCE-PATH to TARGET-INDEX under TARGET-PARENT-PATH."
  (when (and source-path
             (<= (length source-path) (length target-parent-path))
             (equal source-path (subseq target-parent-path 0 (length source-path))))
    (error 'invalid-path-error
           :path source-path
           :tree tree
           :message "Cannot move node into its own subtree"))
  (multiple-value-bind (new-tree node) (pop-node tree source-path)
    (unless node
      (error 'invalid-path-error
             :path source-path
             :tree tree
             :message (fmt "Source node at path ~A not found for move" source-path)))
    (insert-node new-tree target-parent-path target-index node)))

(defun prefix-of-p (prefix path)
  "Return T if list PREFIX is an initial sublist of list PATH."
  (and (<= (length prefix) (length path))
       (equal prefix (subseq path 0 (length prefix)))))

(defun swap-nodes (tree path1 path2)
  "Swap the nodes at PATH1 and PATH2 in a single pass."
  (when (equal path1 path2) (return-from swap-nodes tree))
  (when (or (prefix-of-p path1 path2)
            (prefix-of-p path2 path1))
    (error 'invalid-path-error
           :path (list path1 path2)
           :tree tree
           :message "Cannot swap ancestor and descendant nodes"))
  (let ((node1 (get-node-at-path tree path1))
        (node2 (get-node-at-path tree path2)))
    (unless node1
      (error 'invalid-path-error :path path1 :tree tree :message (fmt "Node not found at path ~A" path1)))
    (unless node2
      (error 'invalid-path-error :path path2 :tree tree :message (fmt "Node not found at path ~A" path2)))
    (labels ((walk (node curr-path)
               (cond
                 ((equal curr-path path1)
                  (reindex-paths node2 curr-path))
                 ((equal curr-path path2)
                  (reindex-paths node1 curr-path))
                 (t
                  (match node
                    ((node p tag children)
                     (list* :path p tag
                            (loop for child in children
                                  for i of-type fixnum from 0
                                  for child-path = (append curr-path (list i))
                                  collect (if (or (prefix-of-p child-path path1)
                                                  (prefix-of-p child-path path2))
                                              (walk child child-path)
                                              child))))
                    (_ node))))))
      (walk tree '()))))

(defun wrap-node (tree path tag)
  "Wrap the node at PATH in a new collection node with TAG (e.g. :paren)."
  (update-node-at-path tree path (lambda (node) (list :path path tag node))))

(defun wrap-range (tree parent-path start-index end-index tag)
  "Wrap the children of PARENT-PATH from START-INDEX to END-INDEX in a new collection with TAG."
  (update-node-at-path
    tree
    parent-path
    (lambda (parent)
      (match parent
             ((node p ptag children)
              (let ((n (length children)))
                (unless (and (>= start-index 0) (<= start-index end-index) (< end-index n) (< start-index n))
                  (error 'invalid-path-error
                         :path parent-path
                         :tree tree
                         :message (fmt "wrap-range indices ~D-~D out of bounds (size ~D) at ~A" start-index end-index n parent-path)))
                (let ((before (take start-index children))
                      (slice (take (1+ (- end-index start-index)) (drop start-index children)))
                      (after (drop (1+ end-index) children)))
                  `(:path ,p ,ptag ,@before (:path ,p ,tag ,@slice) ,@after))))
             (_ parent)))))

(defun unwrap-node (tree path)
  "Unwrap the collection node at PATH, spilling its children into its parent."
  (if (null path)
      tree
      (let ((idx (lastcar path)))
        (update-node-at-path
          tree
          (butlast path)
          (lambda (parent)
            (match parent
                   ((node p ptag children)
                    (unless (and (>= idx 0) (< idx (length children)))
                      (error 'invalid-path-error
                             :path path
                             :tree tree
                             :message (fmt "Child index ~D out of bounds for unwrap at ~A" idx (butlast path))))
                    (let ((target (nth idx children)))
                      (match target
                             ((leaf _ _) parent)
                             ((comment _ _) parent)
                             ((node _ _ inner-children)
                              (reindex-paths
                                `(:path ,p ,ptag ,@(take idx children) ,@inner-children ,@(drop (1+ idx) children))
                                p))
                             (_ parent))))
                   (_ parent)))))))

(defun promote-node (tree path)
  "Promote the node at PATH to replace its parent node."
  (if (null path)
      tree
      (let ((idx (lastcar path)))
        (update-node-at-path
          tree
          (butlast path)
          (lambda (parent)
            (match parent ((node _ _ children) (nth idx children)) (_ parent)))))))

(defun split-node (tree path child-index)
  "Split the collection node at PATH into two siblings at CHILD-INDEX."
  (update-node-at-path
    tree
    path
    (lambda (node)
      (match node
             ((node p tag children)
              (let ((n (length children)))
                (unless (and (>= child-index 0) (<= child-index n))
                  (error 'invalid-path-error
                         :path path
                         :tree tree
                         :message (fmt "split index ~D out of bounds (size ~D) at ~A" child-index n path)))
                (let ((left (take child-index children))
                      (right (drop child-index children)))
                  `(:path ,p ,tag (:path ,p ,tag ,@left) (:path ,p ,tag ,@right)))))
             (_ node)))))

(defun merge-nodes (tree path1 path2)
  "Merge two sibling collection nodes at PATH1 and PATH2 into a single node."
  (when (equal path1 path2)
    (error 'invalid-path-error
           :path (list path1 path2)
           :tree tree
           :message "Cannot merge node with itself"))
  (let ((parent1 (butlast path1))
        (parent2 (butlast path2)))
    (unless (equal parent1 parent2)
      (error 'invalid-path-error
             :path (list path1 path2)
             :tree tree
             :message "Cannot merge nodes with different parents"))
    (let ((left-idx (min (lastcar path1) (lastcar path2)))
          (right-idx (max (lastcar path1) (lastcar path2))))
      (update-node-at-path
        tree
        parent1
        (lambda (parent)
          (match parent
                 ((node p ptag children)
                  (unless (and (>= left-idx 0) (< left-idx (length children))
                               (>= right-idx 0) (< right-idx (length children)))
                    (error 'invalid-path-error
                           :path (list path1 path2)
                           :tree tree
                           :message "Merge indices out of bounds"))
                  (match (list (nth left-idx children) (nth right-idx children))
                         ((list (node p1 tag1 ch1) (node _ _ ch2))
                          `(:path ,p ,ptag ,@(take left-idx children)
                                  (:path ,p1 ,tag1 ,@ch1 ,@ch2)
                                  ,@(take (- right-idx (1+ left-idx)) (drop (1+ left-idx) children))
                                  ,@(drop (1+ right-idx) children)))
                         (_ parent)))
                 (_ parent)))))))
