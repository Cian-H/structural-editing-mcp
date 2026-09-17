(defpackage :structural-editing-mcp.utils
  (:use :cl)
  (:export :insert-at
           :remove-at
           :split-at))

(in-package :structural-editing-mcp.utils)

(defun insert-at (list index value)
  (append (subseq list 0 index)
          (list value)
          (nthcdr index list)))

(defun remove-at (list index)
  (append (subseq list 0 index)
          (nthcdr (1+ index) list)))

(defun split-at (index list)
  (values (subseq list 0 index)
          (nthcdr index list)))
