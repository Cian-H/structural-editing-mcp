(defpackage :structural-editing-mcp.conditions
  (:use :cl)
  (:export :structural-editing-error
           :sexp-parse-error
           :parse-error-token
           :parse-error-message
           :invalid-path-error
           :invalid-path-error-path
           :invalid-path-error-tree
           :workspace-error
           :workspace-error-message)
  (:documentation "Condition hierarchy for the structural editing system."))

(in-package :structural-editing-mcp.conditions)

(define-condition structural-editing-error (error)
  ()
  (:documentation "Base condition for all errors in structural-editing-mcp."))

(define-condition sexp-parse-error (structural-editing-error cl:parse-error)
  ((token :initarg :token
          :reader parse-error-token
          :initform nil)
   (message :initarg :message
            :reader parse-error-message
            :initform nil))
  (:report (lambda (condition stream)
             (format stream "Parse error: ~A~@[ (token: ~S)~]"
                     (or (slot-value condition 'message) "Invalid syntax")
                     (parse-error-token condition))))
  (:documentation "Signaled when parsing s-expressions encounters malformed tokens or unmatched delimiters."))

(define-condition invalid-path-error (structural-editing-error)
  ((path :initarg :path
         :reader invalid-path-error-path
         :initform nil)
   (tree :initarg :tree
         :reader invalid-path-error-tree
         :initform nil)
   (message :initarg :message
            :reader invalid-path-error-message
            :initform nil))
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
