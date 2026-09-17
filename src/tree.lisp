(defpackage :structural-editing-mcp.tree
  (:use :cl
        :trivia
        :alexandria)
  (:export :get-node-at-path
           :reindex-paths
           :update-node-at-path
           :get-node-path
           :get-node-tags
           :get-node-children
           :parse-node))

(in-package :structural-editing-mcp.tree)

(defun get-node-path (node)
  "Return the path list of NODE."
  (second node))

(defun get-node-tags (node)
  "Return the tag keyword (:paren, :leaf, :file, :workspace, etc.) of NODE."
  (third node))

(defun get-node-children (node)
  "Return the children of collection NODE, or NIL if it is a leaf."
  (if (eq (get-node-tags node) :leaf)
      nil
      (cdddr node)))

(defun parse-node (node)
  "Destructure NODE and return (values path tag children-or-leaf-val)."
  (values (get-node-path node)
          (get-node-tags node)
          (if (eq (get-node-tags node) :leaf)
              (fourth node)
              (cdddr node))))

(defun get-node-at-path (tree path)
  "Navigate to the node at PATH in TREE."
  (if (null path)
      tree
      (let ((children (get-node-children tree)))
        (when children
          (let ((next-idx (car path)))
            (when (and (>= next-idx 0) (< next-idx (length children)))
              (get-node-at-path (nth next-idx children) (cdr path))))))))

(defun reindex-paths (tree &optional (current-path '()))
  "Recompute and update all :path metadata in TREE starting at CURRENT-PATH."
  (match tree
    ((list :path _ :leaf val)
     (list :path current-path :leaf val))
    ((list* :path _ tag children)
     (let ((idx 0))
       (list* :path current-path tag
              (mapcar (lambda (child)
                        (prog1 (reindex-paths child (append current-path (list idx)))
                          (incf idx)))
                      children))))
    (_ tree)))

(defun update-node-at-path (tree path fn)
  "Navigate to PATH in TREE, apply FN to the node at PATH, and reconstruct the tree."
  (labels ((walk (node p)
             (if (null p)
                 (funcall fn node)
                 (match node
                   ((list* :path node-path tag children)
                    (let ((child-idx (car p)))
                      (list* :path node-path tag
                             (loop for child in children
                                   for i from 0
                                   collect (if (= i child-idx)
                                               (walk child (cdr p))
                                               child)))))
                   (_ node)))))
    (reindex-paths (walk tree path))))

