(defpackage :structural-editing-mcp.edit
  (:use :cl :trivia :structural-editing-mcp.tree)
  (:export :insert-node
           :overwrite-node
           :delete-node
           :copy-node
           :move-node
           :swap-nodes
           :wrap-node
           :wrap-range
           :unwrap-node
           :promote-node
           :split-node
           :merge-nodes))

(in-package :structural-editing-mcp.edit)

(defun insert-node (tree parent-path index source-string)
  "Parse SOURCE-STRING and insert it into the children of the node at PARENT-PATH at the given INDEX."
  tree)

(defun overwrite-node (tree path source-string)
  "Parse SOURCE-STRING and replace the node at PATH with the resulting tree."
  tree)

(defun delete-node (tree path)
  "Delete the node at PATH."
  tree)

(defun copy-node (tree source-path target-parent-path target-index)
  "Copy the node at SOURCE-PATH and insert it at TARGET-INDEX under TARGET-PARENT-PATH."
  tree)

(defun move-node (tree source-path target-parent-path target-index)
  "Move the node at SOURCE-PATH to TARGET-INDEX under TARGET-PARENT-PATH."
  tree)

(defun swap-nodes (tree path1 path2)
  "Swap the nodes at PATH1 and PATH2."
  tree)

(defun wrap-node (tree path tag)
  "Wrap the node at PATH in a new collection node with TAG (e.g. :paren)."
  tree)

(defun wrap-range (tree parent-path start-index end-index tag)
  "Wrap the children of PARENT-PATH from START-INDEX to END-INDEX in a new collection with TAG."
  tree)

(defun unwrap-node (tree path)
  "Unwrap the collection node at PATH, spilling its children into its parent."
  tree)

(defun promote-node (tree path)
  "Promote the node at PATH to replace its parent node."
  tree)

(defun split-node (tree path child-index)
  "Split the collection node at PATH into two siblings at CHILD-INDEX."
  tree)

(defun merge-nodes (tree path1 path2)
  "Merge two sibling collection nodes at PATH1 and PATH2 into a single node."
  tree)
