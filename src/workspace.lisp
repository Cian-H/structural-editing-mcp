(defpackage :structural-editing-mcp.workspace
  (:use :cl
        :alexandria
        :structural-editing-mcp.parser
        :structural-editing-mcp.tree
        :structural-editing-mcp.conditions)
  (:export :*workspace-tree*
           :*file-registry*
           :*known-dialects*
           :*dialect-extensions*
           :init-workspace
           :file-dialect
           :read-workspace-file
           :write-workspace
           :get-filepath
           :lisp-file-p
           :collect-lisp-files
           :load-into-workspace)
  (:documentation "Project-level multi-file workspace management, file tracking, and disk I/O."))

(in-package :structural-editing-mcp.workspace)

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defparameter *dialect-extensions*
  '(("lisp" . :common-lisp)
    ("cl"   . :common-lisp)
    ("asd"  . :common-lisp)
    ("lsp"  . :common-lisp)
    ("clj"  . :clojure)
    ("cljs" . :clojure)
    ("cljc" . :clojure)
    ("edn"  . :clojure)
    ("scm"  . :scheme)
    ("ss"   . :scheme)
    ("rkt"  . :scheme)
    ("sld"  . :scheme)
    ("el"   . :emacs-lisp)
    ("fnl"  . :fennel))
  "Mapping of file extensions to dialect keywords.")

(defparameter *known-dialects*
  '(:common-lisp :clojure :scheme :emacs-lisp :fennel)
  "List of supported Lisp dialect keywords.")

(defvar *workspace-tree* nil
  "The global AST representing the entire loaded workspace.")

(defvar *file-registry* (make-hash-table :test 'equal)
  "Maps numerical file IDs and tree paths to canonical file paths.")

(defvar *next-file-id* 0
  "Monotonically increasing counter for numerical file IDs.")

(defun init-workspace ()
  "Initialize an empty workspace."
  (setf *file-registry* (make-hash-table :test 'equal))
  (setf *next-file-id* 0)
  (setf *workspace-tree* '(:path () :workspace)))

(defun get-filepath (id-or-path)
  "Get the filepath associated with numerical ID or tree path."
  (let ((key (if (vectorp id-or-path) (coerce id-or-path 'list) id-or-path)))
    (or (gethash key *file-registry*)
        (when (and (listp key) (>= (length key) 2))
          (gethash (subseq key 0 2) *file-registry*)))))

(defun file-dialect (pathname-or-string)
  "Return the dialect keyword (:common-lisp, :clojure, etc.) for PATHNAME-OR-STRING, or NIL if unrecognized."
  (let* ((p (pathname pathname-or-string))
         (type (pathname-type p)))
    (when type
      (cdr (assoc (string-downcase type) *dialect-extensions* :test #'string=)))))

(defun lisp-file-p (pathname-or-string)
  "Return T if PATHNAME-OR-STRING has a recognized Lisp dialect source extension."
  (not (null (file-dialect pathname-or-string))))

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
            thereis (and (stringp path)
                         (let ((true-path (ignore-errors (namestring (truename path)))))
                           (and true-path (string= true-target true-path))))))))

(defun read-workspace-file (filepath)
  "Read a file from disk, parse it, add it to the dialect partition in the workspace tree, and return its ID.
If the file is already loaded, returns its existing ID. Gracefully returns NIL on parse/read failure."
  (unless *workspace-tree* (init-workspace))
  (let* ((canonical-path (or (ignore-errors (namestring (truename filepath))) filepath))
         (dialect (or (file-dialect canonical-path) :common-lisp)))
    (when (file-loaded-p canonical-path)
      (loop for id being the hash-keys of *file-registry*
            using (hash-value path)
            when (and (integerp id)
                      (string= canonical-path (or (ignore-errors (namestring (truename path))) path)))
            do (return-from read-workspace-file id)))
    (handler-case
        (let* ((text (uiop:read-file-string canonical-path))
               (parsed-file-node (string-to-sexp text :dialect dialect))
               (workspace-children (get-node-children *workspace-tree*))
               (new-id *next-file-id*))
          (incf *next-file-id*)
          ;; Locate or create dialect partition node
          (let* ((dialect-pos (position dialect workspace-children :key #'get-node-tag))
                 (d-idx (or dialect-pos (length workspace-children)))
                 (dialect-node (if dialect-pos
                                   (nth dialect-pos workspace-children)
                                   `(:path () ,dialect)))
                 (dialect-files (get-node-children dialect-node))
                 (f-idx (length dialect-files))
                 (updated-dialect-node `(:path () ,dialect ,@dialect-files ,parsed-file-node)))
            (setf (gethash new-id *file-registry*) canonical-path)
            (setf (gethash (list d-idx f-idx) *file-registry*) canonical-path)
            (if dialect-pos
                (let ((new-children (copy-list workspace-children)))
                  (setf (nth dialect-pos new-children) updated-dialect-node)
                  (setf *workspace-tree* `(:path () :workspace ,@new-children)))
                (setf *workspace-tree* `(:path () :workspace ,@workspace-children ,updated-dialect-node)))
            (setf *workspace-tree* (reindex-paths *workspace-tree*))
            new-id))
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
  (unless *workspace-tree*
    (error 'workspace-error :message "No workspace initialized."))
  (dolist (child (get-node-children *workspace-tree*))
    (let ((child-tag (get-node-tag child)))
      (cond
        ((member child-tag *known-dialects*)
         (dolist (file-node (get-node-children child))
           (let* ((path (get-node-path file-node))
                  (filepath (get-filepath path)))
             (when filepath
               (uiop:with-output-file (out filepath :if-exists :supersede :if-does-not-exist :create)
                 (print-sexp file-node out 0 :dialect child-tag))))))
        ((eq child-tag :file)
         (let* ((path (get-node-path child))
                (filepath (or (get-filepath path) (get-filepath (first path)))))
           (when filepath
             (uiop:with-output-file (out filepath :if-exists :supersede :if-does-not-exist :create)
               (print-sexp child out 0 :dialect :common-lisp)))))))))