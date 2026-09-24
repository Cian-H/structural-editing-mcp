(defpackage :structural-editing-mcp.conditions
  (:use :cl)
   (:export :structural-editing-error
           :sexp-parse-error
           :parse-error-token
           :parse-error-message
           :invalid-path-error
           :invalid-path-error-path
           :invalid-path-error-tree
           :invalid-path-error-message
           :workspace-error
           :workspace-error-message
           :workspace-not-found-error
           :workspace-not-found-id
           :workspace-merge-conflict-error
           :merge-conflict-source-id
           :merge-conflict-target-id
           :merge-conflicting-files
           :workspace-dirty-error
           :workspace-dirty-id
           :workspace-dirty-files
           :occ-conflict-error
           :occ-conflict-agent-id
           :occ-conflict-target-path
           :occ-conflict-suggested-read-path
           :occ-conflict-current-revision
           :occ-conflict-agent-revision
           :format-path-notation
           :+error-code-parse-error+
           :+error-code-invalid-params+
           :+error-code-workspace-error+
           :+error-code-occ-conflict+
           :+error-code-internal-error+)
  (:documentation "Condition hierarchy for the structural editing system."))


(in-package :structural-editing-mcp.conditions)

(define-condition structural-editing-error (error)
  ()
  (:documentation "Base condition for all errors in structural-editing-mcp."))

(define-condition sexp-parse-error (structural-editing-error cl:parse-error)
  ((token :initarg :token :reader parse-error-token :initform nil)
   (message :initarg :message :reader parse-error-message :initform nil))
  (:report (lambda (condition stream)
             (format stream "Parse error: ~A~@[ (token: ~S)~]"
                     (or (slot-value condition 'message) "Invalid syntax")
                     (parse-error-token condition))))
  (:documentation "Signaled when parsing s-expressions encounters malformed tokens or unmatched delimiters."))

(define-condition invalid-path-error (structural-editing-error)
  ((path :initarg :path :reader invalid-path-error-path :initform nil)
   (tree :initarg :tree :reader invalid-path-error-tree :initform nil)
   (message :initarg :message :reader invalid-path-error-message :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid path ~A~@[ - ~A~]"
                     (invalid-path-error-path condition)
                     (slot-value condition 'message))))
  (:documentation "Signaled when navigating or updating an AST path that does not exist or is malformed."))

(define-condition workspace-error (structural-editing-error)
  ((message :initarg :message
            :reader workspace-error-message
            :initform "Workspace error"))
  (:report (lambda (condition stream)
             (format stream "Workspace error: ~A" (workspace-error-message condition))))
  (:documentation "Signaled when a workspace operation fails (e.g. uninitialized workspace or missing file)."))

(define-condition workspace-not-found-error (workspace-error)
  ((workspace-id :initarg :workspace-id :reader workspace-not-found-id :initform nil))
  (:report (lambda (condition stream)
             (format stream "Workspace not found: ~S" (workspace-not-found-id condition))))
  (:documentation "Signaled when attempting to access a workspace ID that does not exist."))

(define-condition workspace-merge-conflict-error (workspace-error)
  ((source-id :initarg :source-id :reader merge-conflict-source-id :initform nil)
   (target-id :initarg :target-id :reader merge-conflict-target-id :initform nil)
   (conflicting-files :initarg :conflicting-files :reader merge-conflicting-files :initform nil))
  (:report (lambda (condition stream)
             (format stream "Merge conflict between workspace ~S and ~S: overlapping changes in files: ~{~A~^, ~}"
                     (merge-conflict-source-id condition)
                     (merge-conflict-target-id condition)
                     (merge-conflicting-files condition))))
  (:documentation "Signaled when merging two workspaces encounters conflicting modifications to the same file."))

(define-condition workspace-dirty-error (workspace-error)
  ((workspace-id :initarg :workspace-id :reader workspace-dirty-id :initform nil)
   (dirty-files :initarg :dirty-files :reader workspace-dirty-files :initform nil))
  (:report (lambda (condition stream)
             (format stream "Workspace ~S has uncommitted in-memory changes in ~D file(s): ~{~A~^, ~}. Use force to override."
                     (workspace-dirty-id condition)
                     (length (workspace-dirty-files condition))
                     (workspace-dirty-files condition))))
  (:documentation "Signaled when an operation would overwrite uncommitted in-memory modifications without force."))

(defun format-path-notation (path)
  "Format an AST path into JSON-style bracketed notation e.g. [0, 0, 2] or []."
  (cond
    ((null path) "[]")
    ((listp path) (format nil "[~{~A~^, ~}]" path))
    ((vectorp path) (format nil "[~{~A~^, ~}]" (coerce path 'list)))
    (t (format nil "~A" path))))

(define-condition occ-conflict-error (structural-editing-error)
  ((agent-id :initarg :agent-id :reader occ-conflict-agent-id :initform nil)
   (target-path :initarg :target-path :reader occ-conflict-target-path :initform nil)
   (suggested-read-path :initarg :suggested-read-path :reader occ-conflict-suggested-read-path :initform nil)
   (current-revision :initarg :current-revision :reader occ-conflict-current-revision :initform 1)
   (agent-revision :initarg :agent-revision :reader occ-conflict-agent-revision :initform nil))
  (:report (lambda (condition stream)
             (let ((target-str (format-path-notation (occ-conflict-target-path condition)))
                   (suggested-str (format-path-notation (occ-conflict-suggested-read-path condition)))
                   (cur-rev (occ-conflict-current-revision condition))
                   (agent-rev (occ-conflict-agent-revision condition)))
               (if agent-rev
                 (format stream "Conflict: The workspace was modified by another agent since your last read. Current revision is ~A (your view was at revision ~A). Your target path ~A may have shifted. Action required: Call read_node on ~A to inspect the updated file, re-evaluate your strategy, and submit your edit."
                         cur-rev agent-rev target-str suggested-str)
                 (format stream "Conflict: The workspace was modified by another agent. Current revision is ~A. Your target path ~A may have shifted. Action required: Call read_node on ~A to inspect the updated file, re-evaluate your strategy, and submit your edit."
                         cur-rev target-str suggested-str)))))
  (:documentation "Signaled when an agent's edit conflicts with a concurrent workspace modification."))

(defconstant +error-code-parse-error+ -32700
  "JSON-RPC standard error code for syntax/parse errors.")

(defconstant +error-code-invalid-params+ -32602
  "JSON-RPC standard error code for invalid parameters or invalid AST paths.")

(defconstant +error-code-workspace-error+ -32001
  "JSON-RPC server error code for workspace lifecycle or file access failures.")

(defconstant +error-code-occ-conflict+ -32002
  "JSON-RPC server error code for optimistic concurrency control conflicts.")

(defconstant +error-code-internal-error+ -32603
  "JSON-RPC standard error code for internal server errors.")