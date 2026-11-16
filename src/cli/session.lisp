(in-package :structural-editing-mcp.cli)

(declaim (optimize (speed 2) (safety 3)))

(defparameter *session-dir-name* ".semcp"
                                 "Directory name for persistent CLI workspace session state.")

(defun session-state-file (session-dir)
  "Return the path to state.json inside SESSION-DIR."
  (merge-pathnames "state.json" (uiop:ensure-directory-pathname session-dir)))

(defun find-session-dir (&optional start-dir)
  "Walk up from START-DIR (default CWD) looking for .semcp/state.json.
Returns the .semcp directory pathname if found, or NIL."
  (let
      ((dir (uiop:ensure-directory-pathname (or start-dir (uiop:getcwd)))))
    (loop
      (let*
          ((candidate (merge-pathnames (concatenate 'string *session-dir-name* "/") dir))
           (state-file (session-state-file candidate)))
        (when (probe-file state-file) (return-from find-session-dir candidate)))
      (let
          ((parent (uiop:pathname-parent-directory-pathname dir)))
        (when (or (null parent) (equal parent dir)) (return-from find-session-dir nil))
        (setf dir parent)))))

(defun find-file-coord-in-tree
       (tree registry filepath)
  "Return the (d-idx f-idx) coordinate path of FILEPATH in TREE using REGISTRY."
  (when (and tree (listp tree))
    (loop for d-node in (get-node-children tree)
          for d-idx from 0
          do
          (loop for f-node in (get-node-children d-node)
                for f-idx from 0
                for coord = (list d-idx f-idx)
                when (equal (gethash coord registry) filepath)
                do (return-from find-file-coord-in-tree coord))))
  nil)

(defun serialize-workspace-files (ws)
  "Serialize tracked files in workspace WS into a list of hash-tables."
  (let
      ((tree (workspace-context-tree ws))
       (registry (workspace-context-file-registry ws))
       (clean-state (workspace-context-clean-state ws))
       (file-revs (workspace-context-file-revisions ws))
       (files '()))
    (when (and tree (listp tree))
      (loop for d-node in (get-node-children tree)
            for d-idx from 0
            for dialect = (get-node-tag d-node)
            do
            (loop for f-node in (get-node-children d-node)
                  for f-idx from 0
                  for coord = (list d-idx f-idx)
                  for fpath = (gethash coord registry)
                  when fpath
                  do
                  (let*
                      ((clean-node (gethash fpath clean-state))
                       (clean-p (and clean-node (ast-equal-p f-node clean-node)))
                       (exists-p (and (probe-file fpath) t))
                       (frev (or (gethash fpath file-revs) 1))
                       (entry
                         (dict "path"
                               fpath
                               "dialect"
                               (string-downcase (symbol-name dialect))
                               "clean"
                               (if clean-p
                                 t
                                 yason:false)
                               "revision"
                               frev)))
                    (when (or (not clean-p) (not exists-p))
                      (setf (gethash "content" entry) (sexp-to-string f-node :dialect dialect)))
                    (push entry files)))))
    (nreverse files)))

(defun save-session (session-dir)
  "Persist *WORKSPACE-REGISTRY* state to SESSION-DIR/state.json."
  (let*
      ((dir (uiop:ensure-directory-pathname session-dir))
       (state-file (session-state-file dir))
       (ws-list '()))
    (ensure-directories-exist state-file)
    (bt:with-lock-held (*workspace-registry-lock*)
                       (let ((ids '()))
                         (maphash
                           (lambda (id ws)
                             (declare (ignore ws))
                             (push id ids))
                           *workspace-registry*)
                         (setf ids
                               (sort ids
                                     (lambda (a b)
                                       (cond
                                         ((string= a "default") t)
                                         ((string= b "default") nil)
                                         (t (string< a b))))))
                         (dolist (id ids)
                           (let*
                               ((ws (gethash id *workspace-registry*))
                                (ws-dict
                                  (dict "id"
                                        (workspace-context-id ws)
                                        "parent_id"
                                        (or (workspace-context-parent-id ws) :null)
                                        "base_revision"
                                        (workspace-context-base-revision ws)
                                        "revision"
                                        (workspace-context-revision ws)
                                        "files"
                                        (serialize-workspace-files ws))))
                             (push ws-dict ws-list)))))
    (let
        ((payload (dict "version" 1 "workspaces" (nreverse ws-list))))
      (with-open-file
          (out state-file
               :direction
               :output
               :if-exists
               :supersede
               :if-does-not-exist
               :create
               :external-format
               :utf-8)
        (yason:encode payload out)
        (terpri out)))
    state-file))

(defun restore-workspace-from-dict (ws-dict)
  "Restore a single workspace from its serialized hash-table WS-DICT."
  (let*
      ((ws-id (href ws-dict "id"))
       (ws
         (if (string= ws-id "default")
           (get-workspace "default")
           (create-workspace ws-id)))
       (parent-id (href ws-dict "parent_id"))
       (base-rev (or (href ws-dict "base_revision") 1))
       (rev (or (href ws-dict "revision") 1))
       (files (href ws-dict "files")))
    (setf (workspace-context-parent-id ws)
          (if (or (null parent-id) (eq parent-id :null))
            nil
            parent-id))
    (setf (workspace-context-base-revision ws) base-rev)
    (with-workspace-context (ws)
      (dolist (f-obj files)
        (let*
            ((fpath (href f-obj "path"))
             (dialect-str (href f-obj "dialect"))
             (dialect
               (when (stringp dialect-str) (intern (string-upcase dialect-str) :keyword)))
             (clean-p (href f-obj "clean"))
             (content (href f-obj "content"))
             (frev (or (href f-obj "revision") 1))
             (exists-p (and (probe-file fpath) t)))
          (cond
            ((and clean-p exists-p) (load-into-workspace (list fpath)))
            (exists-p (load-into-workspace (list fpath))
                      (let
                          ((coord (find-file-coord-in-tree *workspace-tree* *file-registry* fpath)))
                        (when (and coord content)
                          (let
                              ((parsed (string-to-sexp content :dialect (or dialect :common-lisp))))
                            (setf *workspace-tree*
                                  (structural-editing-mcp.edit:overwrite-node *workspace-tree* coord parsed))))))
            (t
              (add-file-to-workspace fpath
                                     :content
                                     (or content "")
                                     :dialect
                                     (or dialect :common-lisp)
                                     :ctx
                                     ws)))
          (setf (gethash fpath (workspace-context-file-revisions ws)) frev))))
    (setf (workspace-context-revision ws) rev)
    ws))

(defun reset-workspace-registry ()
  "Reset the default workspace and remove all non-default branches."
  (bt:with-lock-held (*workspace-registry-lock*)
                     (let ((to-remove '()))
                       (maphash
                         (lambda (id ws)
                           (declare (ignore ws))
                           (unless (string= id "default") (push id to-remove)))
                         *workspace-registry*)
                       (dolist (id to-remove) (remhash id *workspace-registry*))))
  (init-workspace (get-workspace "default")))

(defun load-session (session-dir)
  "Load workspace registry state from SESSION-DIR/state.json."
  (let
      ((state-file (session-state-file session-dir)))
    (unless (probe-file state-file)
      (error 'workspace-error
             :message
             (format nil "Session state file not found: ~A" state-file)))
    (let*
        ((yason:*parse-json-arrays-as-vectors* nil)
         (data
           (with-open-file
               (in state-file :direction :input :external-format :utf-8)
             (yason:parse in)))
         (ws-list (href data "workspaces")))
      (reset-workspace-registry)
      (dolist (ws-dict ws-list) (restore-workspace-from-dict ws-dict))
      t)))