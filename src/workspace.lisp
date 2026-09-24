(defpackage :structural-editing-mcp.workspace
  (:use :cl
        :alexandria
        :structural-editing-mcp.parser
        :structural-editing-mcp.tree
        :structural-editing-mcp.conditions)
  (:import-from :serapeum :filter-map :mappend :string-prefix-p)
   (:export :workspace-context
           :make-workspace-context
           :*current-workspace*
           :with-workspace-context
           :*workspace-tree*
           :*file-registry*
           :*file-clean-state*
           :*file-clean-sources*
           :file-clean-p
           :*known-dialects*
           :*dialect-extensions*
           :init-workspace
           :file-dialect
           :read-workspace-file
           :load-into-workspace
           :write-workspace
           :get-filepath
           :lisp-file-p
           :collect-lisp-files
           :workspace-context-id
           :workspace-context-parent-id
           :workspace-context-base-revision
           :workspace-context-tree
           :workspace-context-file-registry
           :workspace-context-clean-state
           :workspace-context-clean-sources
           :workspace-context-next-file-id
           :workspace-context-revision
           :workspace-context-file-revisions
           :workspace-context-lock
           :workspace-context-agent-views
           :workspace-context-snapshots
           :copy-workspace-context
           :*workspace-registry*
           :*workspace-registry-lock*
           :normalize-workspace-id
           :get-workspace
           :create-workspace
           :delete-workspace
           :list-workspaces
           :workspace-dirty-files-list
           :*workspace-lock*
           :*workspace-revision*
           :*agent-views*
           :compute-suggested-read-path
           :record-agent-read
           :validate-agent-edit
           :commit-agent-edit
           :normalize-agent-id
           :safe-truename
           :ensure-workspace)

  (:documentation "Project-level multi-file workspace management, file tracking, and disk I/O."))

(in-package :structural-editing-mcp.workspace)

(declaim (optimize (speed 2) (safety 3)))

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
  structural-editing-mcp.parser:*supported-dialects*
  "List of supported Lisp dialect keywords.")

(defstruct (workspace-context (:constructor make-workspace-context-internal)
                                (:copier nil))
  "Encapsulated workspace context holding AST tree, file registry, and OCC state."
  (id "default" :type string)
  (parent-id nil)
  (base-revision 1 :type fixnum)
  (tree '(:path () :workspace))
  (file-registry (make-hash-table :test 'equal))
  (clean-state (make-hash-table :test 'equal))
  (clean-sources (make-hash-table :test 'equal))
  (next-file-id 0 :type fixnum)
  (revision 1 :type fixnum)
  (file-revisions (make-hash-table :test 'equal))
  (lock (bt:make-lock "workspace-lock"))
  (agent-views (make-hash-table :test 'equal))
  (snapshots (make-hash-table :test 'equal)))

(defun copy-workspace-context (ctx &key (new-id (workspace-context-id ctx)) parent-id base-revision)
  "Create a deep copy of CTX with a new ID."
  (bt:with-lock-held ((workspace-context-lock ctx))
    (make-workspace-context-internal
      :id new-id
      :parent-id (or parent-id (workspace-context-id ctx))
      :base-revision (or base-revision (workspace-context-revision ctx))
      :tree (copy-tree (workspace-context-tree ctx))
      :file-registry (copy-hash-table (workspace-context-file-registry ctx))
      :clean-state (copy-hash-table (workspace-context-clean-state ctx) :test 'equal)
      :clean-sources (copy-hash-table (workspace-context-clean-sources ctx) :test 'equal)
      :next-file-id (workspace-context-next-file-id ctx)
      :revision (workspace-context-revision ctx)
      :file-revisions (copy-hash-table (workspace-context-file-revisions ctx) :test 'equal)
      :lock (bt:make-lock (format nil "workspace-lock-~A" new-id))
      :agent-views (copy-hash-table (workspace-context-agent-views ctx) :test 'equal)
      :snapshots (copy-hash-table (workspace-context-snapshots ctx) :test 'equal))))

(defun make-workspace-context (&key (id "default") parent-id (base-revision 1))
  "Create and return a freshly initialized independent workspace-context."
  (make-workspace-context-internal
    :id id
    :parent-id parent-id
    :base-revision base-revision
    :lock (bt:make-lock (format nil "workspace-lock-~A" id))))

(defvar *workspace-registry-lock* (bt:make-lock "workspace-registry-lock")
  "Lock protecting the global workspace registry.")

(defvar *workspace-registry* (make-hash-table :test 'equal)
  "Global registry mapping workspace IDs (strings) to workspace-context instances.")

(defun normalize-workspace-id (id)
  "Normalize ID to string, defaulting to \"default\" if nil or empty."
  (if (or (null id) (equal id ""))
    "default"
    (string-downcase (string id))))

(defun register-workspace (ctx)
  "Register CTX in *WORKSPACE-REGISTRY* under its ID."
  (bt:with-lock-held (*workspace-registry-lock*)
    (setf (gethash (workspace-context-id ctx) *workspace-registry*) ctx)))

(defparameter *default-workspace* (make-workspace-context :id "default")
  "The default root workspace-context.")

(register-workspace *default-workspace*)

(defvar *current-workspace* *default-workspace*
  "The active workspace-context for the current dynamic extent / thread.")

(defmacro with-workspace-context ((context) &body body)
  "Execute BODY with *CURRENT-WORKSPACE* bound dynamically to CONTEXT."
  `(let ((*current-workspace* ,context))
     ,@body))

(define-symbol-macro *workspace-tree* (workspace-context-tree *current-workspace*))
(define-symbol-macro *file-registry* (workspace-context-file-registry *current-workspace*))
(define-symbol-macro *file-clean-state* (workspace-context-clean-state *current-workspace*))
(define-symbol-macro *file-clean-sources* (workspace-context-clean-sources *current-workspace*))
(define-symbol-macro *next-file-id* (workspace-context-next-file-id *current-workspace*))
(define-symbol-macro *workspace-revision* (workspace-context-revision *current-workspace*))
(define-symbol-macro *workspace-lock* (workspace-context-lock *current-workspace*))
(define-symbol-macro *agent-views* (workspace-context-agent-views *current-workspace*))

(defun get-workspace (&optional (id "default") &key (error-p t))
  "Retrieve workspace-context by ID from registry, optionally signaling workspace-not-found-error."
  (let* ((norm-id (normalize-workspace-id id))
         (ctx (bt:with-lock-held (*workspace-registry-lock*)
                (gethash norm-id *workspace-registry*))))
    (cond
      (ctx ctx)
      (error-p (error 'workspace-not-found-error :workspace-id norm-id))
      (t nil))))

(defun create-workspace (id &key parent-id base-revision (switch-p nil))
  "Create and register a new workspace with ID. If SWITCH-P is true, sets *CURRENT-WORKSPACE*."
  (let ((norm-id (normalize-workspace-id id)))
    (bt:with-lock-held (*workspace-registry-lock*)
      (when (gethash norm-id *workspace-registry*)
        (error 'workspace-error :message (format nil "Workspace ~S already exists" norm-id)))
      (let ((ctx (make-workspace-context :id norm-id
                                         :parent-id parent-id
                                         :base-revision (or base-revision 1))))
        (setf (gethash norm-id *workspace-registry*) ctx)
        (when switch-p
          (setf *current-workspace* ctx))
        ctx))))

(defun delete-workspace (id)
  "Delete workspace by ID from registry. Cannot delete \"default\" workspace."
  (let ((norm-id (normalize-workspace-id id)))
    (when (equal norm-id "default")
      (error 'workspace-error :message "Cannot delete the default workspace"))
    (bt:with-lock-held (*workspace-registry-lock*)
      (unless (gethash norm-id *workspace-registry*)
        (error 'workspace-not-found-error :workspace-id norm-id))
      (remhash norm-id *workspace-registry*))))

(defun workspace-dirty-files-list (&optional (ctx *current-workspace*))
  "Return a list of file paths in CTX that have uncommitted in-memory changes."
  (let ((dirty '()))
    (maphash (lambda (path-or-id filepath)
               (when (and (listp path-or-id) filepath)
                 (let ((file-node (get-node-at-path (workspace-context-tree ctx) path-or-id)))
                   (when (and file-node (not (equal file-node (gethash filepath (workspace-context-clean-state ctx)))))
                     (pushnew filepath dirty :test #'equal)))))
             (workspace-context-file-registry ctx))
    dirty))

(defun list-workspaces ()
  "Return a list of plists describing all registered workspaces."
  (bt:with-lock-held (*workspace-registry-lock*)
    (loop for id being the hash-keys of *workspace-registry*
          using (hash-value ctx)
          collect (list :id id
                        :parent-id (workspace-context-parent-id ctx)
                        :revision (workspace-context-revision ctx)
                        :file-count (hash-table-count (workspace-context-clean-state ctx))
                        :dirty-p (not (null (workspace-dirty-files-list ctx)))))))

(defun init-workspace (&optional (ctx *current-workspace*))
  "Initialize or reset CTX as an empty workspace."
  (bt:with-lock-held ((workspace-context-lock ctx))
    (setf (workspace-context-file-registry ctx) (make-hash-table :test 'equal))
    (setf (workspace-context-clean-state ctx) (make-hash-table :test 'equal))
    (setf (workspace-context-clean-sources ctx) (make-hash-table :test 'equal))
    (setf (workspace-context-next-file-id ctx) 0)
    (setf (workspace-context-revision ctx) 1)
    (setf (workspace-context-agent-views ctx) (make-hash-table :test 'equal))
    (setf (workspace-context-tree ctx) '(:path () :workspace))))

(defun normalize-agent-id (agent-id)
  "Return AGENT-ID, defaulting to \"default\" if nil or empty string."
  (if (or (null agent-id) (equal agent-id ""))
    "default"
    agent-id))

(defun safe-truename (path)
  "Return the truename of PATH as a namestring, or NIL on error."
  (ignore-errors (namestring (truename path))))

(defun ensure-workspace ()
  "Initialize workspace if not already loaded."
  (unless *workspace-tree* (init-workspace)))

(defun compute-suggested-read-path (target-path)
  "Compute the parent file or dialect path from TARGET-PATH to inspect upon conflict."
  (cond
    ((null target-path) nil)
    ((>= (length target-path) 2) (subseq target-path 0 2))
    (t target-path)))

(defun record-agent-read (&optional (agent-id "default"))
  "Record that AGENT-ID has observed the current *WORKSPACE-REVISION*."
  (let ((id (normalize-agent-id agent-id)))
    (setf (gethash id *agent-views*) *workspace-revision*)))

(defun validate-agent-edit (&key (agent-id "default") target-path)
  "Validate that AGENT-ID is editing against the current *WORKSPACE-REVISION*.
Signals OCC-CONFLICT-ERROR if the workspace has changed since the agent's last read or edit."
  (let* ((id (normalize-agent-id agent-id))
         (agent-rev (gethash id *agent-views*)))
    (when (or (and agent-rev (/= agent-rev *workspace-revision*))
              (and (null agent-rev) (> *workspace-revision* 1)))
      (error 'occ-conflict-error
             :agent-id id
             :target-path target-path
             :suggested-read-path (compute-suggested-read-path target-path)
             :current-revision *workspace-revision*
             :agent-revision agent-rev))
    t))

(defun commit-agent-edit (&optional (agent-id "default"))
  "Advance *WORKSPACE-REVISION* and update AGENT-ID's view to the new revision."
  (let ((id (normalize-agent-id agent-id)))
    (incf *workspace-revision*)
    (setf (gethash id *agent-views*) *workspace-revision*)
    *workspace-revision*))

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
  (let ((name (lastcar (pathname-directory dir-pathname))))
    (and name
         (or (string-prefix-p "." name)
             (member (string-downcase name)
                     '("target" "node_modules" "fasl" "dist" "build" "bin" "obj")
                     :test #'string=)))))

(defun scan-directory-lisp-files (dir)
  "Recursively scan DIR for all Lisp files, skipping ignored directories."
  (let ((result '()))
    (labels ((walk (d)
               (unless (ignored-dir-p d)
                 (dolist (f (uiop:directory-files d))
                   (when (lisp-file-p f)
                     (push (namestring (truename f)) result)))
                 (dolist (sub (uiop:subdirectories d))
                   (walk sub)))))
      (walk dir)
      (nreverse result))))

(defun collect-lisp-files (path)
  "Recursively collect all Lisp filepaths under PATH (if a directory) or return PATH (if a Lisp file).
Non-Lisp files, ignored directories, and non-existent paths return NIL."
  (let ((p (probe-file path)))
    (cond
      ((null p) nil)
      ((uiop:directory-pathname-p p)
        (scan-directory-lisp-files p))
      ((lisp-file-p p)
        (list (namestring (truename p))))
      (t nil))))

(defun file-loaded-p (filepath)
  "Return T if FILEPATH is already tracked in *FILE-REGISTRY*."
  (let ((true-target (safe-truename filepath)))
    (some (lambda (path)
            (and (stringp path)
                 (or (string= filepath path)
                     (and true-target
                          (let ((true-path (safe-truename path)))
                            (and true-path (string= true-target true-path)))))))
          (hash-table-values *file-registry*))))

(defun find-loaded-file-id (canonical-path)
  "Return existing file ID for CANONICAL-PATH from *FILE-REGISTRY*, or NIL."
  (find-if (lambda (id)
             (let ((path (gethash id *file-registry*)))
               (string= canonical-path (or (safe-truename path) path))))
           (remove-if-not #'integerp (hash-table-keys *file-registry*))))

(defun insert-file-into-workspace (parsed-file-node canonical-path dialect)
  "Register and insert PARSED-FILE-NODE into *WORKSPACE-TREE* under DIALECT partition.
Returns the newly assigned numerical file ID."
  (let* ((workspace-children (get-node-children *workspace-tree*))
         (new-id *next-file-id*)
         (dialect-pos (position dialect workspace-children :key #'get-node-tag))
         (d-idx (or dialect-pos (length workspace-children)))
         (dialect-node (if dialect-pos
                         (nth dialect-pos workspace-children)
                         `(:path () ,dialect)))
         (dialect-files (get-node-children dialect-node))
         (f-idx (length dialect-files))
         (updated-dialect-node `(:path () ,dialect ,@dialect-files ,parsed-file-node)))
    (incf *next-file-id*)
    (setf (gethash new-id *file-registry*) canonical-path)
    (setf (gethash (list d-idx f-idx) *file-registry*) canonical-path)
    (if dialect-pos
      (let ((new-children (copy-list workspace-children)))
        (setf (nth dialect-pos new-children) updated-dialect-node)
        (setf *workspace-tree* `(:path () :workspace ,@new-children)))
      (setf *workspace-tree* `(:path () :workspace ,@workspace-children ,updated-dialect-node)))
    (setf *workspace-tree* (reindex-paths *workspace-tree*))
    new-id))

(defun find-file-path-coords (canonical-path)
  "Locate the workspace tree coordinate path for CANONICAL-PATH."
  (loop for k being the hash-keys of *file-registry*
        using (hash-value v)
        when (and (listp k) (equal v canonical-path))
        return k))

(defun register-clean-file-state (canonical-path parsed-file-node)
  "Record clean snapshot of PARSED-FILE-NODE for CANONICAL-PATH."
  (let* ((coords (find-file-path-coords canonical-path))
         (reindexed (and coords (get-node-at-path *workspace-tree* coords))))
    (setf (gethash canonical-path *file-clean-state*)
          (copy-tree (or reindexed parsed-file-node)))))

(defun parse-and-register-file (canonical-path text dialect)
  "Parse TEXT into AST, insert into workspace, and record clean source state."
  (multiple-value-bind (parsed-file-node toplevel-sources)
                       (string-to-sexp text :dialect dialect)
    (setf (gethash canonical-path *file-clean-sources*) toplevel-sources)
    (let ((id (insert-file-into-workspace parsed-file-node canonical-path dialect)))
      (register-clean-file-state canonical-path parsed-file-node)
      id)))

(defun try-load-file-text (canonical-path filepath dialect)
  "Read and register file contents, returning file ID or NIL on failure."
  (handler-case
      (let ((text (uiop:read-file-string canonical-path)))
        (parse-and-register-file canonical-path text dialect))
    (error (c)
      (format *error-output* "~&[Workspace] Warning: failed to load ~A: ~A~%" filepath c)
     nil)))

(defun read-workspace-file (filepath)
  "Read a file from disk, parse it, add it to the dialect partition in the workspace tree, and return its ID.
If the file is already loaded, returns its existing ID. Gracefully returns NIL on parse/read failure."
  (ensure-workspace)
  (let* ((canonical-path (or (safe-truename filepath) filepath))
         (dialect (or (file-dialect canonical-path) :common-lisp)))
    (or (find-loaded-file-id canonical-path)
        (try-load-file-text canonical-path filepath dialect))))

(defun load-into-workspace (paths)
  "Given a list of file/directory paths (or a single path), expand directories,
filter for Lisp files, and load them into the workspace tree.
Returns a list of loaded numerical file IDs."
  (ensure-workspace)
  (let ((files (remove-duplicates (mappend #'collect-lisp-files (ensure-list paths)) :test #'equal)))
    (filter-map #'read-workspace-file files)))

(defun file-clean-p (file-node &optional fallback-path)
  "Return T if FILE-NODE is structurally identical to its clean loaded state."
  (let* ((path (get-node-path file-node))
         (filepath (or (get-filepath path) (when fallback-path (get-filepath fallback-path)))))
    (and filepath
         (let ((clean-node (gethash filepath *file-clean-state*)))
           (and clean-node (equal file-node clean-node))))))

(defun update-written-file-clean-state (filepath file-node dialect)
  "Re-parse written file on disk to update clean state and clean sources."
  (let ((written-text (uiop:read-file-string filepath)))
    (multiple-value-bind (re-parsed new-sources)
                         (string-to-sexp written-text :dialect dialect)
      (declare (ignore re-parsed))
      (setf (gethash filepath *file-clean-state*) (copy-tree file-node))
      (setf (gethash filepath *file-clean-sources*) new-sources))))

(defun write-file-node-to-disk (file-node dialect &optional fallback-path)
  "Write a single :file node to its registered filepath on disk."
  (let* ((path (get-node-path file-node))
         (filepath (or (get-filepath path) (when fallback-path (get-filepath fallback-path)))))
    (when filepath
      (let ((clean-node (gethash filepath *file-clean-state*))
            (clean-sources (gethash filepath *file-clean-sources*)))
        (uiop:with-output-file (out filepath :if-exists :supersede :if-does-not-exist :create)
                               (if (and clean-node clean-sources)
                                 (structural-editing-mcp.parser:print-file-with-clean-sources file-node clean-node clean-sources out dialect)
                                 (print-sexp file-node out 0 :dialect dialect)))
        (update-written-file-clean-state filepath file-node dialect)))))

(defun write-dirty-dialect-files (dialect-node dialect)
  "Write all modified file nodes under DIALECT-NODE to disk."
  (dolist (file-node (get-node-children dialect-node))
    (unless (file-clean-p file-node)
      (write-file-node-to-disk file-node dialect))))

(defun write-workspace ()
  "Write all modified :file nodes in the workspace back to disk. Clean files are skipped."
  (unless *workspace-tree*
    (error 'workspace-error :message "No workspace initialized."))
  (dolist (child (get-node-children *workspace-tree*))
    (let ((child-tag (get-node-tag child)))
      (cond
        ((member child-tag *known-dialects*)
          (write-dirty-dialect-files child child-tag))
        ((eq child-tag :file)
          (let ((fallback (first (get-node-path child))))
            (unless (file-clean-p child fallback)
              (write-file-node-to-disk child :common-lisp fallback))))))))