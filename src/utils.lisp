(defpackage :structural-editing-mcp.utils
  (:use :cl)
  (:export :insert-at
           :remove-at
           :split-at)
  (:documentation "Pure list manipulation utilities for structural editing."))

(in-package :structural-editing-mcp.utils)

(declaim (inline insert-at remove-at split-at))

(defun insert-at (list index value)
  "Insert VALUE into LIST at 0-indexed position INDEX."
  (declare (type list list) (type fixnum index))
  (append (subseq list 0 index) (list value) (nthcdr index list)))

(defun remove-at (list index)
  "Remove the element at 0-indexed position INDEX from LIST."
  (declare (type list list) (type fixnum index))
  (append (subseq list 0 index) (nthcdr (1+ index) list)))

(defun split-at (index list)
  "Split LIST at 0-indexed position INDEX, returning (values before after)."
  (declare (type list list) (type fixnum index))
  (values (subseq list 0 index) (nthcdr index list)))