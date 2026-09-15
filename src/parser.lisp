(defpackage :structural-editing-mcp.parser
  (:use :cl)
  (:export :string-to-sexp
           :sexp-to-string))

(in-package :structural-editing-mcp.parser)

(defun string-to-sexp (string)
  "Parse a raw Lisp string into an s-expression data structure."
  (read-from-string string))

(defun sexp-to-string (expr)
  "Serialize an s-expression back into its string representation."
  (write-to-string expr :escape nil))
