(defpackage :structural-editing-mcp.workspace
  (:use :cl
        :structural-editing-mcp.parser
        :structural-editing-mcp.tree)
  (:export :*workspace-tree*
           :*file-registry*
           :init-workspace
           :read-workspace-file
           :write-workspace
           :get-filepath))

(in-package :structural-editing-mcp.workspace)

(defvar *workspace-tree* nil
  "The global AST representing the entire loaded workspace.")

(defvar *file-registry* (make-hash-table :test 'equal)
  "Maps numerical file IDs (child indices) to file paths.")

(defun init-workspace ()
  "Initialize an empty workspace."
  (setf *file-registry* (make-hash-table :test 'equal))
  (setf *workspace-tree* (list :path '() :workspace)))

(defun get-filepath (id)
  "Get the filepath associated with the numerical ID."
  (gethash id *file-registry*))

(defun read-workspace-file (filepath)
  "Read a file from disk, parse it, add it to the workspace tree, and return its ID."
  (unless *workspace-tree*
    (init-workspace))
  (let* ((text (uiop:read-file-string filepath))
         (parsed-file-node (string-to-sexp text))
         (existing-children (get-node-children *workspace-tree*))
         (new-id (length existing-children)))
    (setf (gethash new-id *file-registry*) filepath)
    ;; Append the new file node as a child of the workspace
    (setf *workspace-tree*
          (list* :path '() :workspace (append existing-children (list parsed-file-node))))
    ;; Re-index paths from the root to ensure everything has correct paths
    (setf *workspace-tree* (reindex-paths *workspace-tree*))
    new-id))

(defun write-workspace ()
  "Write all :file nodes in the workspace back to their respective paths on disk."
  (unless *workspace-tree*
    (error "No workspace initialized."))
  (let ((file-nodes (get-node-children *workspace-tree*)))
    (loop for file-node in file-nodes
          for id from 0
          for filepath = (get-filepath id)
          do (when filepath
               (let ((text (sexp-to-string file-node)))
                 (uiop:with-output-file (out filepath :if-exists :supersede :if-does-not-exist :create)
                   (write-string text out)))))))
