(defpackage :structural-editing-mcp.workspace
    (:use
        :cl
        :alexandria
        :structural-editing-mcp.parser
        :structural-editing-mcp.tree
        :structural-editing-mcp.conditions)
  (:export
           :*workspace-tree*
           :*file-registry*
           :init-workspace
           :read-workspace-file
           :write-workspace
           :get-filepath
           :lisp-file-p
           :collect-lisp-files
           :load-into-workspace)
  (:documentation
                  "Project-level multi-file workspace management, file tracking, and disk I/O."))

(in-package :structural-editing-mcp.workspace)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defvar *workspace-tree* nil
  "The global AST representing the entire loaded workspace.")

(defvar *file-registry* (make-hash-table :test 'equal)
  "Maps numerical file IDs (child indices) to file paths.")

(defun init-workspace ()
  "Initialize an empty workspace."
  (setf *file-registry* (make-hash-table :test 'equal))
  (setf *workspace-tree* ' (:path () :workspace)))

(defun get-filepath (id)
  "Get the filepath associated with the numerical ID."
  (gethash id *file-registry*))

(defun lisp-file-p (pathname-or-string)
  "Return T if PATHNAME-OR-STRING has a recognized Lisp source extension."
  (let* ((p (pathname pathname-or-string))
         (type (pathname-type p)))
    (and type (member (string-downcase type) '("lisp" "cl" "asd" "lsp") :test #'string=))))

(defun ignored-dir-p (dir-pathname)
  "Return T if DIR-PATHNAME should be ignored when walking directories."
  (let* ((dir-list (pathname-directory dir-pathname))
         (name (car (last dir-list))))
    (and name
         (or (and (> (length name) 0) (char= (char name 0) #\.))
             (member (string-downcase name)
                     '("target" "node_modules" "fasl" "dist" "build" "bin" "obj")
                     :test #'string=)))))

(defun collect-lisp-files (path)
  "Recursively collect all Lisp filepaths under PATH (if a directory) or return PATH (if a Lisp file).
Non-Lisp files, ignored directories, and non-existent paths return NIL."
  (let ((p (probe-file path)))
    (unless p (return-from collect-lisp-files nil))
    (cond
      ((uiop:directory-pathname-p p)
       (let ((result '()))
         (labels ((walk (dir)
                    (unless (ignored-dir-p dir)
                      (dolist (f (uiop:directory-files dir))
                        (when (lisp-file-p f)
                          (push (namestring (truename f)) result)))
                      (dolist (sub (uiop:subdirectories dir))
                        (walk sub)))))
           (walk p)
           (nreverse result))))
      ((lisp-file-p p)
       (list (namestring (truename p))))
      (t nil))))

(defun file-loaded-p (filepath)
  "Return T if FILEPATH is already tracked in *FILE-REGISTRY*."
  (let ((true-target (ignore-errors (namestring (truename filepath)))))
    (when true-target
      (loop for path being the hash-values of *file-registry*
            for true-path = (ignore-errors (namestring (truename path)))
            thereis (and true-path (string= true-target true-path))))))

(defun read-workspace-file (filepath)
  "Read a file from disk, parse it, add it to the workspace tree, and return its ID.
If the file is already loaded, returns its existing ID. Gracefully returns NIL on parse/read failure."
  (unless *workspace-tree* (init-workspace))
  (let ((canonical-path (or (ignore-errors (namestring (truename filepath))) filepath)))
    (when (file-loaded-p canonical-path)
      (loop for id being the hash-keys of *file-registry*
            using (hash-value path)
            when (string= canonical-path (or (ignore-errors (namestring (truename path))) path))
            do (return-from read-workspace-file id)))
    (handler-case
        (let* ((text (uiop:read-file-string canonical-path))
               (parsed-file-node (string-to-sexp text))
               (existing-children (get-node-children *workspace-tree*))
               (new-id (length existing-children)))
          (setf (gethash new-id *file-registry*) canonical-path)
          (setf *workspace-tree*
                `(:path () :workspace ,@existing-children ,parsed-file-node))
          (setf *workspace-tree* (reindex-paths *workspace-tree*))
          new-id)
      (error (c)
        (format *error-output* "~&[Workspace] Warning: failed to load ~A: ~A~%" filepath c)
        nil))))

(defun load-into-workspace (paths)
  "Given a list of file/directory paths (or a single path), expand directories,
filter for Lisp files, and load them into the workspace tree.
Returns a list of loaded numerical file IDs."
  (unless *workspace-tree* (init-workspace))
  (let ((loaded-ids '()))
    (dolist (path (alexandria:ensure-list paths))
      (let ((files (collect-lisp-files path)))
        (dolist (f files)
          (let ((id (read-workspace-file f)))
            (when id (pushnew id loaded-ids))))))
    (nreverse loaded-ids)))

(defun write-workspace ()
  "Write all :file nodes in the workspace back to their respective paths on disk."
  (unless
          *workspace-tree*
          (error 'workspace-error :message "No workspace initialized."))
  (loop
        for
        file-node
        in
        (get-node-children *workspace-tree*)
        for
        id
        from
        0
        for
        filepath
        =
        (get-filepath id)
        do
        (when
          filepath
          (uiop:with-output-file
        (out filepath :if-exists :supersede :if-does-not-exist :create)
        (print-sexp file-node out 0)))))