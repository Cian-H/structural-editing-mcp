(defpackage :structural-editing-mcp.tree
  (:use :cl
        :trivia
        :alexandria)
  (:export :get-node-at-path
           :reindex-paths
           :update-node-at-path
           :get-node-path
           :get-node-tag
           :get-node-tags
           :get-node-children
           :parse-node
           :node
           :leaf)
  (:documentation "Core AST representations, patterns, accessors, and path navigation."))

(in-package :structural-editing-mcp.tree)

(defpattern node (path tag children)
  `(list* :path ,path ,tag ,children))

(defpattern leaf (path val)
  `(list :path ,path :leaf ,val))

(declaim (inline get-node-path get-node-tag get-node-tags get-node-children))

(defun get-node-path (node)
  "Return the path list of NODE, or NIL if invalid."
  (match node
    ((or (node path _ _)
         (leaf path _))
     path)
    (_ nil)))

(defun get-node-tag (node)
  "Return the tag keyword (:paren, :leaf, :file, :workspace, etc.) of NODE."
  (match node
    ((leaf _ _) :leaf)
    ((node _ tag _) tag)
    (_ nil)))

(defun get-node-tags (node)
  "Return the tag keyword of NODE (alias for GET-NODE-TAG)."
  (get-node-tag node))

(defun get-node-children (node)
  "Return the children of collection NODE, or NIL if it is a leaf or invalid."
  (match node
    ((node _ _ children) children)
    (_ nil)))

(defun parse-node (node)
  "Destructure NODE and return (values path tag children-or-leaf-val)."
  (match node
    ((leaf path val)
     (values path :leaf val))
    ((node path tag children)
     (values path tag children))
    (_ (values nil nil nil))))

(defun get-node-at-path (tree path)
  "Navigate to the node at PATH in TREE, or NIL if not found."
  (loop with current = tree
        for idx in path
        for children = (get-node-children current)
        for sub = (and children (>= idx 0) (nthcdr idx children))
        if sub
          do (setf current (car sub))
        else
          return nil
        finally (return current)))

(defun reindex-paths (tree &optional (current-path '()))
  "Recompute and update all :path metadata in TREE starting at CURRENT-PATH."
  (match tree
    ((leaf _ val)
     (list :path current-path :leaf val))
    ((node _ tag children)
     (list* :path current-path tag
            (loop for child in children
                  for idx from 0
                  for child-path = (append current-path (list idx))
                  collect (reindex-paths child child-path))))
    (_ tree)))

(defun update-node-at-path (tree path fn)
  "Navigate to PATH in TREE, apply FN to the node at PATH, and reconstruct the tree."
  (labels ((walk (elem p)
             (if (null p)
                 (funcall fn elem)
                 (match elem
                   ((node node-path tag children)
                    (let ((child-idx (car p)))
                      (list* :path node-path tag
                             (loop for child in children
                                   for i from 0
                                   collect (if (= i child-idx)
                                               (walk child (cdr p))
                                               child)))))
                   (_ elem)))))
    (reindex-paths (walk tree path))))

