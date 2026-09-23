(defpackage :structural-editing-mcp.tree
  (:use :cl :trivia :alexandria :structural-editing-mcp.conditions)
  (:import-from :serapeum :fmt)
  (:export :get-node-at-path
           :reindex-paths
           :update-node-at-path
           :get-node-path
           :get-node-tag
           :get-node-children
           :get-node-leaf-value
           :compound-node-p
           :resolve-tree-scope
           :parse-node
           :node
           :leaf
           :comment)
  (:documentation "Core AST representations, patterns, accessors, and path navigation."))

(in-package :structural-editing-mcp.tree)

(defpattern node (path tag children)
            `(list* :path ,path ,tag ,children))

(defpattern leaf (path val)
            `(list :path ,path :leaf ,val))

(defpattern comment (path text)
            `(list :path ,path :comment ,text))

(declaim (inline get-node-path get-node-tag get-node-children compound-node-p))

(defun get-node-path (node)
  "Return the path list of NODE, or NIL if invalid."
  (match node
         ((or (node path _ _) (leaf path _) (comment path _)) path)
         (_ nil)))

(defun get-node-tag (node)
  "Return the tag keyword (:paren, :leaf, :comment, :file, :workspace, etc.) of NODE."
  (match node
         ((leaf _ _) :leaf)
         ((comment _ _) :comment)
         ((node _ tag _) tag)
         (_ nil)))

(defun get-node-children (node)
  "Return the children of collection NODE, or NIL if it is a leaf or invalid."
  (match node
         ((or (leaf _ _) (comment _ _)) nil)
         ((node _ _ children) children)
         (_ nil)))

(defun get-node-leaf-value (node)
  "Return the value of leaf NODE, or NIL if it is not a leaf."
  (match node
         ((leaf _ val) val)
         (_ nil)))

(defun compound-node-p (node)
  "Return T if NODE is a compound parenthesized or bracketed collection."
  (let ((tag (get-node-tag node)))
    (or (eq tag :paren) (eq tag :square))))

(defun parse-node (node)
  "Destructure NODE and return (values path tag children-or-leaf-val)."
  (match node
         ((leaf path val) (values path :leaf val))
         ((comment path text) (values path :comment text))
         ((node path tag children) (values path tag children))
         (_ (values nil nil nil))))

(defun get-node-at-path (tree path)
  "Navigate to the node at PATH in TREE, or NIL if not found."
  (declare (type list path))
  (loop with current = tree
        for idx of-type fixnum in path
        for children = (get-node-children current)
        for sub = (and children (>= idx 0) (nthcdr idx children))
        if sub
        do (setf current (car sub))
        else
        return nil
        finally (return current)))

(defun resolve-tree-scope (tree path)
  "Resolve PATH against TREE. If PATH is nil or empty, returns TREE; otherwise returns the node at PATH."
  (if (and path (not (null path)))
    (get-node-at-path tree path)
    tree))

(defun reindex-paths (tree &optional (current-path '()))
  "Recompute and update all :path metadata in TREE starting at CURRENT-PATH."
  (declare (type list current-path))
  (labels ((reindex-aux (node rev-path)
             (let ((full-path (reverse rev-path)))
               (match node
                      ((leaf _ val)
                       (list :path full-path :leaf val))
                      ((comment _ text)
                       (list :path full-path :comment text))
                      ((node _ tag children)
                       (list* :path full-path tag
                              (loop for child in children
                                    for idx of-type fixnum from 0
                                    collect (reindex-aux child (cons idx rev-path)))))
                      (_ node)))))
    (reindex-aux tree (reverse current-path))))

(defun update-node-at-path (tree path fn)
  "Navigate to PATH in TREE, apply FN to the node at PATH, and reconstruct the tree.
Only re-indexes the modified subtree at PATH rather than traversing the entire tree root."
  (declare (type list path))
  (labels ((walk (elem p curr-path)
             (if (null p)
               (reindex-paths (funcall fn elem) curr-path)
               (match elem
                      ((node node-path tag children)
                       (let ((child-idx (car p)))
                         (if (or (< child-idx 0) (>= child-idx (length children)))
                           (error 'invalid-path-error
                                  :path path
                                  :tree tree
                                  :message (fmt "Child index ~D out of bounds for node at path ~A" child-idx node-path))
                           (list* :path node-path tag
                                  (loop for child in children
                                        for i of-type fixnum from 0
                                        collect (if (= i child-idx)
                                                  (walk child (cdr p) (append curr-path (list i)))
                                                  child))))))
                      (_ (error 'invalid-path-error
                                :path path
                                :tree tree
                                :message (fmt "Cannot navigate into non-node element at path ~A" curr-path)))))))
    (walk tree path '())))