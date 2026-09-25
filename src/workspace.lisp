(defpackage :structural-editing-mcp.workspace
  (:use :cl
        :alexandria
        :structural-editing-mcp.parser
        :structural-editing-mcp.tree
        :structural-editing-mcp.conditions)
  (:import-from :serapeum :filter-map :mappend :string-prefix-p)
  (:import-from :structural-editing-mcp.edit :overwrite-node)
  (:export :workspace-context
           :make-workspace-context
           :*current-workspace*
           :with-workspace-context
           :with-workspaces-locked
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
           :clear-workspace
           :fork-workspace
           :snapshot-workspace
           :restore-workspace
           :reload-workspace
           :diff-workspaces
           :merge-workspaces
           :merge-file-ast
           :rebase-workspace
           :add-file-to-workspace
           :workspace-status
           :workspace-dirty-files-list
           :workspace-all-files-list
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

(defun copy-workspace-context-unlocked (ctx &key (new-id (workspace-context-id ctx)) parent-id base-revision)
  "Create a deep copy of CTX without acquiring lock (caller must hold lock or ensure exclusivity)."
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
    :snapshots (copy-hash-table (workspace-context-snapshots ctx) :test 'equal)))

(defun copy-workspace-context (ctx &key (new-id (workspace-context-id ctx)) parent-id base-revision)
  "Create a deep copy of CTX with a new ID, acquiring CTX's lock."
  (bt:with-lock-held ((workspace-context-lock ctx))
                     (copy-workspace-context-unlocked ctx :new-id new-id :parent-id parent-id :base-revision base-revision)))

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

(defmacro with-workspaces-locked ((ctx1 ctx2) &body body)
  "Acquire locks for both CTX1 and CTX2 in a deterministic order based on workspace ID.
If CTX1 and CTX2 share the same lock, acquires the lock only once."
  (let ((c1 (gensym "CTX1"))
        (c2 (gensym "CTX2"))
        (l1 (gensym "LOCK1"))
        (l2 (gensym "LOCK2"))
        (id1 (gensym "ID1"))
        (id2 (gensym "ID2")))
    `(let* ((,c1 ,ctx1)
            (,c2 ,ctx2)
            (,l1 (workspace-context-lock ,c1))
            (,l2 (workspace-context-lock ,c2))
            (,id1 (workspace-context-id ,c1))
            (,id2 (workspace-context-id ,c2)))
       (cond
         ((eq ,l1 ,l2)
           (bt:with-lock-held (,l1)
                              ,@body))
         ((or (string< ,id1 ,id2)
              (and (string= ,id1 ,id2)
                   (< (sxhash ,l1) (sxhash ,l2))))
           (bt:with-lock-held (,l1)
                              (bt:with-lock-held (,l2)
                                                 ,@body)))
         (t
           (bt:with-lock-held (,l2)
                              (bt:with-lock-held (,l1)
                                                 ,@body)))))))

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

(defun workspace-all-files-list (&optional (ctx *current-workspace*))
  "Return a list of all file paths tracked in CTX (both clean and uncommitted)."
  (let ((files '()))
    (maphash (lambda (path-or-id filepath)
               (when (and (listp path-or-id) filepath)
                 (pushnew filepath files :test #'equal)))
             (workspace-context-file-registry ctx))
    files))

(defun add-file-to-workspace (filepath &key (content "") dialect (ctx *current-workspace*))
  "Add a new file FILEPATH with initial CONTENT and optional DIALECT to CTX in memory without touching disk.
The file is immediately marked as uncommitted (dirty) until committed."
  (let* ((canonical-path (or (safe-truename filepath) filepath))
         (eff-dialect (or dialect (file-dialect canonical-path) :common-lisp)))
    (with-workspace-context (ctx)
      (bt:with-lock-held ((workspace-context-lock ctx))
                         (when (find-file-path-coords canonical-path)
                           (error 'workspace-error :message (format nil "File ~A already exists in workspace ~A"
                                                                    filepath (workspace-context-id ctx))))
                         (multiple-value-bind (parsed-file-node sources)
                                              (string-to-sexp content :dialect eff-dialect)
                           (setf (gethash canonical-path *file-clean-sources*) sources)
                           (let ((id (insert-file-into-workspace parsed-file-node canonical-path eff-dialect)))
                             (incf (workspace-context-revision ctx))
                             (values id (find-file-path-coords canonical-path))))))))

(defun list-workspaces ()
  "Return a list of plists describing all registered workspaces."
  (let ((workspaces
          (bt:with-lock-held (*workspace-registry-lock*)
                             (loop for ctx being the hash-values of *workspace-registry*
                                   collect ctx))))
    (loop for ctx in workspaces
          collect (bt:with-lock-held ((workspace-context-lock ctx))
                                     (list :id (workspace-context-id ctx)
                                           :parent-id (workspace-context-parent-id ctx)
                                           :revision (workspace-context-revision ctx)
                                           :file-count (length (workspace-all-files-list ctx))
                                           :dirty-p (not (null (workspace-dirty-files-list ctx))))))))

(defun init-workspace (&optional (ctx *current-workspace*))
  "Initialize or reset CTX as an empty workspace."
  (bt:with-lock-held ((workspace-context-lock ctx))
                     (setf (workspace-context-file-registry ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-clean-state ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-clean-sources ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-next-file-id ctx) 0)
                     (setf (workspace-context-revision ctx) 1)
                     (setf (workspace-context-agent-views ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-snapshots ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-tree ctx) '(:path () :workspace))))

(defun clear-workspace (&optional (ctx *current-workspace*) &key force)
  "Reset CTX to an empty workspace. If FORCE is nil and uncommitted changes exist, signals WORKSPACE-DIRTY-ERROR."
  (bt:with-lock-held ((workspace-context-lock ctx))
                     (let ((dirty (workspace-dirty-files-list ctx)))
                       (when (and dirty (not force))
                         (error 'workspace-dirty-error
                                :workspace-id (workspace-context-id ctx)
                                :dirty-files dirty
                                :message (format nil "Cannot clear workspace ~S: has uncommitted modifications in ~S. Pass force: true to discard."
                                                 (workspace-context-id ctx) dirty))))
                     (setf (workspace-context-file-registry ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-clean-state ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-clean-sources ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-next-file-id ctx) 0)
                     (setf (workspace-context-revision ctx) 1)
                     (setf (workspace-context-agent-views ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-snapshots ctx) (make-hash-table :test 'equal))
                     (setf (workspace-context-tree ctx) '(:path () :workspace))
                     t))

(defun fork-workspace (source-id target-id)
  "Fork SOURCE-ID workspace into a newly created independent TARGET-ID workspace."
  (let* ((src (get-workspace source-id))
         (norm-target (normalize-workspace-id target-id)))
    (bt:with-lock-held (*workspace-registry-lock*)
                       (when (gethash norm-target *workspace-registry*)
                         (error 'workspace-error :message (format nil "Workspace ~S already exists" norm-target)))
                       (let ((forked (copy-workspace-context src
                                                             :new-id norm-target
                                                             :parent-id (workspace-context-id src)
                                                             :base-revision (workspace-context-revision src))))
                         (setf (gethash norm-target *workspace-registry*) forked)
                         forked))))

(defun snapshot-workspace (snapshot-name &optional (ctx *current-workspace*))
  "Save an in-memory deep copy snapshot of CTX under SNAPSHOT-NAME."
  (let ((name (string snapshot-name)))
    (bt:with-lock-held ((workspace-context-lock ctx))
                       (let ((snap (copy-workspace-context-unlocked ctx :new-id (format nil "~A-snapshot-~A" (workspace-context-id ctx) name))))
                         (setf (gethash name (workspace-context-snapshots ctx)) snap)
                         name))))

(defun restore-workspace (snapshot-name &optional (ctx *current-workspace*))
  "Restore workspace state of CTX from the saved in-memory snapshot SNAPSHOT-NAME."
  (let ((name (string snapshot-name)))
    (bt:with-lock-held ((workspace-context-lock ctx))
                       (let ((snap (gethash name (workspace-context-snapshots ctx))))
                         (unless snap
                           (error 'workspace-error :message (format nil "Snapshot ~S not found in workspace ~S"
                                                                    name (workspace-context-id ctx))))
                         (setf (workspace-context-tree ctx) (copy-tree (workspace-context-tree snap)))
                         (setf (workspace-context-file-registry ctx) (copy-hash-table (workspace-context-file-registry snap)))
                         (setf (workspace-context-clean-state ctx) (copy-hash-table (workspace-context-clean-state snap) :test 'equal))
                         (setf (workspace-context-clean-sources ctx) (copy-hash-table (workspace-context-clean-sources snap) :test 'equal))
                         (setf (workspace-context-next-file-id ctx) (workspace-context-next-file-id snap))
                         (setf (workspace-context-revision ctx) (workspace-context-revision snap))
                         (setf (workspace-context-file-revisions ctx) (copy-hash-table (workspace-context-file-revisions snap) :test 'equal))
                         (setf (workspace-context-agent-views ctx) (copy-hash-table (workspace-context-agent-views snap) :test 'equal))
                         name))))

(defun reload-workspace (&key (ctx *current-workspace*) files force)
  "Reload files from disk into CTX. If FILES is nil, reloads all files currently in CTX.
If FORCE is nil and uncommitted dirty files would be overwritten, signals WORKSPACE-DIRTY-ERROR."
  (bt:with-lock-held ((workspace-context-lock ctx))
                     (let* ((dirty (workspace-dirty-files-list ctx))
                            (all-loaded (loop for k being the hash-keys of (workspace-context-clean-state ctx) collect k))
                            (target-files (if files
                                            (mapcar (lambda (f) (or (safe-truename f) f)) (ensure-list files))
                                            all-loaded))
                            (conflicting-dirty (intersection dirty target-files :test #'equal)))
                       (when (and conflicting-dirty (not force))
                         (error 'workspace-dirty-error
                                :workspace-id (workspace-context-id ctx)
                                :dirty-files conflicting-dirty
                                :message (format nil "Cannot reload: uncommitted changes in files ~S. Pass force: true to discard."
                                                 conflicting-dirty)))
                       (with-workspace-context (ctx)
                         (dolist (f target-files)
                           (when (probe-file f)
                             (let ((dialect (or (file-dialect f) :common-lisp))
                                   (text (uiop:read-file-string f))
                                   (coords (find-file-path-coords f)))
                               (multiple-value-bind (parsed-node sources)
                                                    (string-to-sexp text :dialect dialect)
                                 (setf (gethash f *file-clean-sources*) sources)
                                 (if coords
                                   (progn
                                     (setf *workspace-tree* (overwrite-node *workspace-tree* coords parsed-node))
                                     (register-clean-file-state f parsed-node))
                                   (parse-and-register-file f text dialect)))))))
                       (incf (workspace-context-revision ctx))
                       target-files)))

(defun workspace-status (&optional (ctx *current-workspace*))
  "Return a plist detailing current state of CTX: id, revision, dirty files, clean files, and snapshots."
  (bt:with-lock-held ((workspace-context-lock ctx))
                     (let* ((dirty (workspace-dirty-files-list ctx))
                            (all-files (loop for k being the hash-keys of (workspace-context-clean-state ctx) collect k))
                            (clean (set-difference all-files dirty :test #'equal))
                            (snaps (loop for k being the hash-keys of (workspace-context-snapshots ctx) collect k)))
                       (list :id (workspace-context-id ctx)
                             :parent-id (workspace-context-parent-id ctx)
                             :revision (workspace-context-revision ctx)
                             :base-revision (workspace-context-base-revision ctx)
                             :dirty-files dirty
                             :clean-files clean
                             :snapshots snaps))))

(defun get-file-node-in-workspace (ctx filepath)
  "Locate the file AST node in CTX corresponding to FILEPATH, or NIL."
  (let ((coords (with-workspace-context (ctx) (find-file-path-coords filepath))))
    (when coords
      (get-node-at-path (workspace-context-tree ctx) coords))))

(defun reconcile-file-forms
       (max-len base-forms src-forms tgt-forms)
  (let
      ((merged-forms '()) (conflicts '()) (conflict-p nil))
    (dotimes (i max-len)
      (let
          ((b (nth i base-forms)) (s (nth i src-forms)) (tg (nth i tgt-forms)))
        (cond
          ((and (equal s b) (equal tg b)) (when b (push (copy-tree b) merged-forms)))
          ((and (not (equal s b)) (equal tg b))
            (when s (push (copy-tree s) merged-forms)))
          ((and (equal s b) (not (equal tg b)))
            (when tg (push (copy-tree tg) merged-forms)))
          ((equal s tg) (when s (push (copy-tree s) merged-forms)))
          (t (setf conflict-p t) (push i conflicts)))))
    (values merged-forms conflict-p conflicts)))

(defun merge-file-ast
       (base-file src-file tgt-file)
  "Perform a 3-way AST merge of SRC-FILE and TGT-FILE against BASE-FILE at the top-level form level.
Returns (values merged-file-node conflict-p conflicting-indices)."
  (cond
    ((equal src-file tgt-file) (values (copy-tree src-file) nil nil))
    ((null base-file) (values nil t '(:no-base)))
    (t
      (let*
          ((base-forms (get-node-children base-file))
           (src-forms (get-node-children src-file))
           (tgt-forms (get-node-children tgt-file))
           (path (or (get-node-path tgt-file) (get-node-path src-file) '()))
           (tag (or (get-node-tag tgt-file) (get-node-tag src-file) :file))
           (max-len (max (length base-forms) (length src-forms) (length tgt-forms))))
        (multiple-value-bind
            (merged-forms conflict-p conflicts)
            (reconcile-file-forms max-len base-forms src-forms tgt-forms)
          (if conflict-p
            (values nil t (nreverse conflicts))
            (let
                ((merged-node (list* :path path tag (nreverse merged-forms))))
              (values (reindex-paths merged-node path) nil nil))))))))

(defun diff-workspaces-unlocked (src tgt)
  "Compare workspaces SRC and TGT without acquiring locks (caller must hold locks). Returns a plist summarizing discrepancies."
  (let*
      ((src-files (workspace-all-files-list src))
       (tgt-files (workspace-all-files-list tgt))
       (src-dirty (workspace-dirty-files-list src))
       (tgt-dirty (workspace-dirty-files-list tgt))
       (source-only (set-difference src-files tgt-files :test #'equal))
       (target-only (set-difference tgt-files src-files :test #'equal))
       (common-files (intersection src-files tgt-files :test #'equal))
       (both-dirty (intersection src-dirty tgt-dirty :test #'equal))
       (source-modified-only
         (intersection src-dirty
                       (set-difference common-files tgt-dirty :test #'equal)
                       :test
                       #'equal))
       (target-modified-only
         (intersection tgt-dirty
                       (set-difference common-files src-dirty :test #'equal)
                       :test
                       #'equal))
       (ast-differing '())
       (ast-mergeable '())
       (colliding '())
       (conflict-details '()))
    (dolist (f common-files)
      (let
          ((node-src (get-file-node-in-workspace src f))
           (node-tgt (get-file-node-in-workspace tgt f)))
        (when
            (and node-src node-tgt (not (equal node-src node-tgt)))
          (push f ast-differing))))
    (dolist (f both-dirty)
      (let
          ((base-node
             (or
               (gethash f (workspace-context-clean-state tgt))
               (gethash f (workspace-context-clean-state src))))
           (src-node (get-file-node-in-workspace src f))
           (tgt-node (get-file-node-in-workspace tgt f)))
        (multiple-value-bind
            (merged-node conflict-p conflict-indices)
            (merge-file-ast base-node src-node tgt-node)
          (declare (ignore merged-node))
          (if conflict-p
            (progn (push f colliding)
                   (push (list :file f :conflicts conflict-indices) conflict-details))
            (push f ast-mergeable)))))
    (list :source-id
          (workspace-context-id src)
          :target-id
          (workspace-context-id tgt)
          :source-only
          source-only
          :target-only
          target-only
          :source-modified-only
          source-modified-only
          :target-modified-only
          target-modified-only
          :ast-mergeable
          ast-mergeable
          :modified-in-both
          colliding
          :conflict-details
          conflict-details
          :ast-differing-files
          ast-differing)))

(defun diff-workspaces (source-id target-id)
  "Compare workspaces SOURCE-ID and TARGET-ID. Returns a plist summarizing discrepancies."
  (let*
      ((src (get-workspace source-id)) (tgt (get-workspace target-id)))
    (with-workspaces-locked (src tgt) (diff-workspaces-unlocked src tgt))))

(defun copy-file-between-workspaces
       (src-ctx tgt-ctx filepath)
  "Transfer file FILEPATH from SRC-CTX into TGT-CTX."
  (let
      ((node (get-file-node-in-workspace src-ctx filepath))
       (dialect (or (file-dialect filepath) :common-lisp))
       (clean-sources (gethash filepath (workspace-context-clean-sources src-ctx)))
       (clean-state (gethash filepath (workspace-context-clean-state src-ctx))))
    (unless node
      (error 'workspace-error
             :message
             (format nil
                     "File ~A not found in workspace ~A"
                     filepath
                     (workspace-context-id src-ctx))))
    (with-workspace-context (tgt-ctx)
      (let ((coords (find-file-path-coords filepath)))
        (if coords
          (progn
            (setf *workspace-tree*
                  (overwrite-node *workspace-tree* coords (copy-tree node)))
            (setf (gethash filepath *file-clean-sources*) clean-sources))
          (multiple-value-bind (parsed-node sources)
                               (string-to-sexp (sexp-to-string node :dialect dialect) :dialect dialect)
            (declare (ignore parsed-node))
            (setf (gethash filepath *file-clean-sources*) sources)
            (insert-file-into-workspace (copy-tree node) filepath dialect)))
        (if clean-state
          (setf (gethash filepath *file-clean-state*) (copy-tree clean-state))
          (remhash filepath *file-clean-state*)))
      (setf *workspace-tree* (reindex-paths *workspace-tree*)))))

(defun merge-ast-files-into-workspace
       (tgt src ast-merge-files)
  "Merge 3-way AST nodes for AST-MERGE-FILES from SRC into TGT workspace."
  (dolist (f ast-merge-files)
    (let*
        ((base-node
           (or
             (gethash f (workspace-context-clean-state tgt))
             (gethash f (workspace-context-clean-state src))))
         (src-node (get-file-node-in-workspace src f))
         (tgt-node (get-file-node-in-workspace tgt f))
         (merged-node (merge-file-ast base-node src-node tgt-node)))
      (with-workspace-context (tgt)
        (let ((coords (find-file-path-coords f)))
          (when coords
            (setf *workspace-tree* (overwrite-node *workspace-tree* coords merged-node))
            (setf *workspace-tree* (reindex-paths *workspace-tree*))))))))

(defun merge-workspaces
       (source-id target-id &key files (strategy :fast-forward-or-disjoint))
  "Merge SOURCE-ID into TARGET-ID.
If FILES is provided, transfers only those files.
Otherwise performs fast-forward or disjoint/AST-level merge, signaling WORKSPACE-MERGE-CONFLICT-ERROR on conflicting modifications."
  (declare (ignore strategy))
  (let*
      ((src (get-workspace source-id)) (tgt (get-workspace target-id)))
    (when
        (equal (workspace-context-id src) (workspace-context-id tgt))
      (error 'workspace-error
             :message
             (format nil "Cannot merge workspace ~S into itself" (workspace-context-id src))))
    (with-workspaces-locked (src tgt)
      (let ((diff (diff-workspaces-unlocked src tgt)))
        (cond
          ;; Selective file merge
          (files
            (let
                ((target-files
                   (mapcar
                     (lambda (f)
                       (or (safe-truename f) f))
                     (ensure-list files))))
              (dolist (f target-files) (copy-file-between-workspaces src tgt f))
              (incf (workspace-context-revision tgt))
              (list :action "selective" :merged-files target-files)))
          ;; Fast-forward: target has not moved since fork and has not loaded new files or changes
          ((and
             (equal (workspace-context-parent-id src) (workspace-context-id tgt))
             (= (workspace-context-revision tgt) (workspace-context-base-revision src))
             (null (workspace-dirty-files-list tgt))
             (null (getf diff :target-only)))
            (setf (workspace-context-tree tgt) (copy-tree (workspace-context-tree src)))
            (setf (workspace-context-file-registry tgt)
                  (copy-hash-table (workspace-context-file-registry src)))
            (setf (workspace-context-clean-state tgt)
                  (copy-hash-table (workspace-context-clean-state src) :test 'equal))
            (setf (workspace-context-clean-sources tgt)
                  (copy-hash-table (workspace-context-clean-sources src) :test 'equal))
            (setf (workspace-context-next-file-id tgt) (workspace-context-next-file-id src))
            (setf (workspace-context-revision tgt) (workspace-context-revision src))
            (list :action
                  "fast-forward"
                  :merged-files
                  (or (getf diff :source-modified-only) (getf diff :source-only))))
          ;; Disjoint files and AST-mergeable files
          (t
            (when (getf diff :modified-in-both)
              (error 'workspace-merge-conflict-error
                     :source-id
                     (workspace-context-id src)
                     :target-id
                     (workspace-context-id tgt)
                     :conflicting-files
                     (getf diff :modified-in-both)
                     :conflicting-details
                     (getf diff :conflict-details)))
            (let
                ((files-to-merge
                   (append (getf diff :source-only) (getf diff :source-modified-only)))
                 (ast-merge-files (getf diff :ast-mergeable)))
              (dolist (f files-to-merge) (copy-file-between-workspaces src tgt f))
              (merge-ast-files-into-workspace tgt src ast-merge-files)
              (incf (workspace-context-revision tgt))
              (list :action
                    (if ast-merge-files
                      "disjoint-and-ast-merge"
                      "disjoint")
                    :merged-files
                    (append files-to-merge ast-merge-files)))))))))

(defun rebase-workspace
       (source-id &key (onto-id "default") (strategy :error))
  "Rebase SOURCE-ID workspace onto ONTO-ID workspace.
Transfers upstream files, applies non-conflicting upstream changes, and performs 3-way AST merges on modified files.
STRATEGY can be :error (signal on collision), :theirs (accept onto version), or :ours (keep source version)."
  (let*
      ((src (get-workspace source-id))
       (onto (get-workspace onto-id))
       (norm-strategy
         (if (stringp strategy)
           (intern (string-upcase strategy) :keyword)
           strategy)))
    (when
        (equal (workspace-context-id src) (workspace-context-id onto))
      (error 'workspace-error
             :message
             (format nil "Cannot rebase workspace ~S onto itself" (workspace-context-id src))))
    (with-workspaces-locked (src onto)
      (let*
          ((diff (diff-workspaces-unlocked src onto))
           (onto-only (getf diff :target-only))
           (onto-modified (getf diff :target-modified-only))
           (both-modified
             (append (getf diff :ast-mergeable) (getf diff :modified-in-both)))
           (conflicts (getf diff :modified-in-both))
           (conflict-details (getf diff :conflict-details))
           (updated-files '()))
        (when (and conflicts (eq norm-strategy :error))
          (error 'workspace-merge-conflict-error
                 :source-id
                 (workspace-context-id src)
                 :target-id
                 (workspace-context-id onto)
                 :conflicting-files
                 conflicts
                 :conflicting-details
                 conflict-details))
        (dolist (f onto-only)
          (copy-file-between-workspaces onto src f)
          (push f updated-files))
        (dolist (f onto-modified)
          (copy-file-between-workspaces onto src f)
          (push f updated-files))
        (dolist (f both-modified)
          (let*
              ((base-node
                 (or
                   (gethash f (workspace-context-clean-state onto))
                   (gethash f (workspace-context-clean-state src))))
               (src-node (get-file-node-in-workspace src f))
               (onto-node (get-file-node-in-workspace onto f)))
            (multiple-value-bind (merged-node conflict-p)
                                 (merge-file-ast base-node src-node onto-node)
              (let
                  ((chosen-node
                     (cond
                       ((not conflict-p) merged-node)
                       ((eq norm-strategy :theirs) (copy-tree onto-node))
                       ((eq norm-strategy :ours) (copy-tree src-node))
                       (t (copy-tree onto-node)))))
                (with-workspace-context (src)
                  (let ((coords (find-file-path-coords f)))
                    (when coords
                      (setf *workspace-tree* (overwrite-node *workspace-tree* coords chosen-node))
                      (setf *workspace-tree* (reindex-paths *workspace-tree*))))
                  (push f updated-files))))))
        (setf (workspace-context-parent-id src) (workspace-context-id onto))
        (setf (workspace-context-base-revision src) (workspace-context-revision onto))
        (incf (workspace-context-revision src))
        (list :action
              "rebase"
              :source-id
              (workspace-context-id src)
              :onto-id
              (workspace-context-id onto)
              :updated-files
              (remove-duplicates updated-files :test #'equal)
              :strategy
              norm-strategy)))))

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

(defun record-agent-read
       (&optional (agent-id "default"))
  "Record that AGENT-ID has observed the current *WORKSPACE-REVISION*."
  (let ((id (normalize-agent-id agent-id)))
    (setf (gethash id *agent-views*) *workspace-revision*)))

(defun validate-agent-edit
       (&key (agent-id "default") target-path)
  "Validate that AGENT-ID is editing against the current *WORKSPACE-REVISION*.
Signals OCC-CONFLICT-ERROR if the workspace has changed since the agent's last read or edit."
  (let*
      ((id (normalize-agent-id agent-id)) (agent-rev (gethash id *agent-views*)))
    (when
        (or
          (and agent-rev (/= agent-rev *workspace-revision*))
          (and (null agent-rev) (> *workspace-revision* 1)))
      (error 'occ-conflict-error
             :agent-id
             id
             :target-path
             target-path
             :suggested-read-path
             (compute-suggested-read-path target-path)
             :current-revision
             *workspace-revision*
             :agent-revision
             agent-rev))
    t))

(defun commit-agent-edit
       (&optional (agent-id "default"))
  "Advance *WORKSPACE-REVISION* and update AGENT-ID's view to the new revision."
  (let ((id (normalize-agent-id agent-id)))
    (incf *workspace-revision*)
    (setf (gethash id *agent-views*) *workspace-revision*)
    *workspace-revision*))

(defun get-filepath (id-or-path)
  "Get the filepath associated with numerical ID or tree path."
  (let
      ((key
         (if (vectorp id-or-path)
           (coerce id-or-path 'list)
           id-or-path)))
    (or (gethash key *file-registry*)
        (when (and (listp key) (>= (length key) 2))
          (gethash (subseq key 0 2) *file-registry*)))))

(defun file-dialect (pathname-or-string)
  "Return the dialect keyword (:common-lisp, :clojure, etc.) for PATHNAME-OR-STRING, or NIL if unrecognized."
  (let*
      ((p (pathname pathname-or-string)) (type (pathname-type p)))
    (when type
      (cdr (assoc (string-downcase type) *dialect-extensions* :test #'string=)))))

(defun lisp-file-p (pathname-or-string)
  "Return T if PATHNAME-OR-STRING has a recognized Lisp dialect source extension."
  (not (null (file-dialect pathname-or-string))))

(defun ignored-dir-p (dir-pathname)
  "Return T if DIR-PATHNAME should be ignored when walking directories."
  (let
      ((name (lastcar (pathname-directory dir-pathname))))
    (and name
         (or (string-prefix-p "." name)
             (member (string-downcase name)
                     '
                     ("target" "node_modules" "fasl" "dist" "build" "bin" "obj")
                     :test
                     #'string=)))))

(defun scan-directory-lisp-files (dir)
  "Recursively scan DIR for all Lisp files, skipping ignored directories."
  (let ((result '()))
    (labels
        ((walk (d)
           (unless (ignored-dir-p d)
             (dolist (f (uiop:directory-files d))
               (when (lisp-file-p f) (push (namestring (truename f)) result)))
             (dolist (sub (uiop:subdirectories d)) (walk sub)))))
      (walk dir)
      (nreverse result))))

(defun collect-lisp-files (path)
  "Recursively collect all Lisp filepaths under PATH (if a directory) or return PATH (if a Lisp file).
Non-Lisp files, ignored directories, and non-existent paths return NIL."
  (let ((p (probe-file path)))
    (cond
      ((null p) nil)
      ((uiop:directory-pathname-p p) (scan-directory-lisp-files p))
      ((lisp-file-p p) (list (namestring (truename p))))
      (t nil))))

(defun file-loaded-p (filepath)
  "Return T if FILEPATH is already tracked in *FILE-REGISTRY*."
  (let ((true-target (safe-truename filepath)))
    (some
      (lambda (path)
        (and (stringp path)
             (or (string= filepath path)
                 (and true-target
                      (let ((true-path (safe-truename path)))
                        (and true-path (string= true-target true-path)))))))
      (hash-table-values *file-registry*))))

(defun find-loaded-file-id (canonical-path)
  "Return existing file ID for CANONICAL-PATH from *FILE-REGISTRY*, or NIL."
  (find-if
    (lambda (id)
      (let ((path (gethash id *file-registry*)))
        (string= canonical-path (or (safe-truename path) path))))
    (remove-if-not #'integerp (hash-table-keys *file-registry*))))

(defun insert-file-into-workspace
       (parsed-file-node canonical-path dialect)
  "Register and insert PARSED-FILE-NODE into *WORKSPACE-TREE* under DIALECT partition.
Returns the newly assigned numerical file ID."
  (let*
      ((workspace-children (get-node-children *workspace-tree*))
       (new-id *next-file-id*)
       (dialect-pos (position dialect workspace-children :key #'get-node-tag))
       (d-idx (or dialect-pos (length workspace-children)))
       (dialect-node
         (if dialect-pos
           (nth dialect-pos workspace-children)
           `
           (:path () ,dialect)))
       (dialect-files (get-node-children dialect-node))
       (f-idx (length dialect-files))
       (updated-dialect-node ` (:path () ,dialect ,@dialect-files ,parsed-file-node)))
    (incf *next-file-id*)
    (setf (gethash new-id *file-registry*) canonical-path)
    (setf (gethash (list d-idx f-idx) *file-registry*) canonical-path)
    (if dialect-pos
      (let
          ((new-children (copy-list workspace-children)))
        (setf (nth dialect-pos new-children) updated-dialect-node)
        (setf *workspace-tree* ` (:path () :workspace ,@new-children)))
      (setf *workspace-tree*
            `
            (:path () :workspace ,@workspace-children ,updated-dialect-node)))
    (setf *workspace-tree* (reindex-paths *workspace-tree*))
    new-id))

(defun find-file-path-coords (canonical-path)
  "Locate the workspace tree coordinate path for CANONICAL-PATH."
  (loop for
        k
        being
        the
        hash-keys
        of
        *file-registry*
        using
        (hash-value v)
        when
        (and (listp k) (equal v canonical-path))
        return
        k))

(defun register-clean-file-state
       (canonical-path parsed-file-node)
  "Record clean snapshot of PARSED-FILE-NODE for CANONICAL-PATH."
  (let*
      ((coords (find-file-path-coords canonical-path))
       (reindexed (and coords (get-node-at-path *workspace-tree* coords))))
    (setf (gethash canonical-path *file-clean-state*)
          (copy-tree (or reindexed parsed-file-node)))))

(defun parse-and-register-file
       (canonical-path text dialect)
  "Parse TEXT into AST, insert into workspace, and record clean source state."
  (multiple-value-bind
      (parsed-file-node toplevel-sources)
      (string-to-sexp text :dialect dialect)
    (setf (gethash canonical-path *file-clean-sources*) toplevel-sources)
    (let
        ((id (insert-file-into-workspace parsed-file-node canonical-path dialect)))
      (register-clean-file-state canonical-path parsed-file-node)
      id)))

(defun try-load-file-text
       (canonical-path filepath dialect)
  "Read and register file contents, returning file ID or NIL on failure."
  (handler-case
      (let
          ((text (uiop:read-file-string canonical-path)))
        (parse-and-register-file canonical-path text dialect))
    (error (c)
      (format *error-output*
              "~&[Workspace] Warning: failed to load ~A: ~A~%"
              filepath
              c)
     nil)))

(defun read-workspace-file (filepath)
  "Read a file from disk, parse it, add it to the dialect partition in the workspace tree, and return its ID.
If the file is already loaded, returns its existing ID. Gracefully returns NIL on parse/read failure."
  (ensure-workspace)
  (let*
      ((canonical-path (or (safe-truename filepath) filepath))
       (dialect (or (file-dialect canonical-path) :common-lisp)))
    (or (find-loaded-file-id canonical-path)
        (try-load-file-text canonical-path filepath dialect))))

(defun load-into-workspace (paths)
  "Given a list of file/directory paths (or a single path), expand directories,
filter for Lisp files, and load them into the workspace tree.
Returns a list of loaded numerical file IDs."
  (ensure-workspace)
  (let
      ((files
         (remove-duplicates
           (mappend #'collect-lisp-files (ensure-list paths))
           :test
           #'equal)))
    (filter-map #'read-workspace-file files)))

(defun file-clean-p
       (file-node &optional fallback-path)
  "Return T if FILE-NODE is structurally identical to its clean loaded state."
  (let*
      ((path (get-node-path file-node))
       (filepath
         (or (get-filepath path) (when fallback-path (get-filepath fallback-path)))))
    (and filepath
         (let
             ((clean-node (gethash filepath *file-clean-state*)))
           (and clean-node (equal file-node clean-node))))))

(defun update-written-file-clean-state
       (filepath file-node dialect)
  "Re-parse written file on disk to update clean state and clean sources."
  (let
      ((written-text (uiop:read-file-string filepath)))
    (multiple-value-bind (re-parsed new-sources)
                         (string-to-sexp written-text :dialect dialect)
      (declare (ignore re-parsed))
      (setf (gethash filepath *file-clean-state*) (copy-tree file-node))
      (setf (gethash filepath *file-clean-sources*) new-sources))))

(defun write-file-node-to-disk
       (file-node dialect &optional fallback-path)
  "Write a single :file node to its registered filepath on disk."
  (let*
      ((path (get-node-path file-node))
       (filepath
         (or (get-filepath path) (when fallback-path (get-filepath fallback-path)))))
    (when filepath
      (let
          ((clean-node (gethash filepath *file-clean-state*))
           (clean-sources (gethash filepath *file-clean-sources*)))
        (uiop:with-output-file
          (out filepath :if-exists :supersede :if-does-not-exist :create)
          (if (and clean-node clean-sources)
            (structural-editing-mcp.parser:print-file-with-clean-sources
              file-node
              clean-node
              clean-sources
              out
              dialect)
            (print-sexp file-node out 0 :dialect dialect)))
        (update-written-file-clean-state filepath file-node dialect)))))

(defun write-dirty-dialect-files
       (dialect-node dialect &optional allowed-files)
  "Write modified file nodes under DIALECT-NODE to disk, filtering by ALLOWED-FILES if non-nil."
  (dolist
      (file-node (get-node-children dialect-node))
    (let
        ((fp (get-filepath (get-node-path file-node))))
      (when
          (and fp
               (or (null allowed-files) (member fp allowed-files :test #'equal))
               (not (file-clean-p file-node)))
        (write-file-node-to-disk file-node dialect)))))

(defun write-workspace (&optional files)
  "Write modified :file nodes in the workspace back to disk. Clean files are skipped.
If FILES is provided, commits only matching files."
  (unless *workspace-tree*
    (error 'workspace-error :message "No workspace initialized."))
  (let
      ((allowed
         (when files
           (mapcar
             (lambda (f)
               (or (safe-truename f) f))
             (ensure-list files)))))
    (dolist
        (child (get-node-children *workspace-tree*))
      (let ((child-tag (get-node-tag child)))
        (cond
          ((member child-tag *known-dialects*)
            (write-dirty-dialect-files child child-tag allowed))
          ((eq child-tag :file)
            (let*
                ((fallback (first (get-node-path child)))
                 (fp
                   (or (get-filepath (get-node-path child))
                       (when fallback (get-filepath fallback)))))
              (when
                  (and fp
                       (or (null allowed) (member fp allowed :test #'equal))
                       (not (file-clean-p child fallback)))
                (write-file-node-to-disk child :common-lisp fallback)))))))))