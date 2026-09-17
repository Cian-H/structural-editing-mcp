(defpackage :structural-editing-mcp.tree
  (:use :cl :trivia)
  (:export :get-node-at-path
           :reindex-paths))

(in-package :structural-editing-mcp.tree)

(defun get-node-at-path (tree path)
  "Navigate to the node at PATH in TREE."
  (if (null path)
      tree
      (match tree
        ((list* :path _ (or :paren :square :curly) children)
         (let ((next-idx (car path)))
           (when (and (>= next-idx 0) (< next-idx (length children)))
             (get-node-at-path (nth next-idx children) (cdr path)))))
        (_ nil))))

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
