(defpackage :structural-editing-mcp.paredit
  (:use :cl :trivia)
  (:export :slurp-left
           :slurp-right
           :barf-left
           :barf-right
           :wrap
           :splice
           :raise
           :convolute
           :split
           :join))

(in-package :structural-editing-mcp.paredit)

(defun slurp-left (expr &optional context)
  "Move opening parenthesis outward."
  expr)

(defun slurp-right (expr &optional context)
  "Move closing parenthesis outward."
  expr)

(defun barf-left (expr &optional context)
  "Move opening parenthesis inward."
  expr)

(defun barf-right (expr &optional context)
  "Move closing parenthesis inward."
  expr)

(defun wrap (expr wrapper)
  "Wrap expr in wrapper."
  expr)

(defun splice (expr &optional context)
  "Remove the enclosing parentheses around the current expression."
  expr)

(defun raise (expr &optional context)
  "Replace the parent form entirely with the current form."
  expr)

(defun convolute (expr &optional context)
  "Reverse the nesting order of the current list and its parent list."
  expr)

(defun split (expr &optional context)
  "Split a single list into two separate sibling lists."
  expr)

(defun join (expr &optional context)
  "Merge two adjacent sibling lists into a single unified form."
  expr)
