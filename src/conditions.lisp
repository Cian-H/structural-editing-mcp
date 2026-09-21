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
           :occ-conflict-error
           :occ-conflict-agent-id
           :occ-conflict-target-path
           :occ-conflict-suggested-read-path
           :occ-conflict-current-revision
           :occ-conflict-agent-revision
           :format-path-notation)
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