#!/usr/bin/env -S devenv shell -- sbcl --noinform --disable-debugger --script
;;; Re-indent project Lisp sources with cl-indentify.
;;; Usage: ./scripts/format.lisp

(require 'asdf)
(asdf:load-system :cl-indentify)
(indentify:initialize-templates)

(let* ((root (uiop:pathname-parent-directory-pathname
               (uiop:pathname-directory-pathname
                 (or *load-truename* *load-pathname*))))
       (files (append (directory (merge-pathnames "src/*.lisp" root))
                      (directory (merge-pathnames "test/*.lisp" root))
                      (directory (merge-pathnames "scripts/*.lisp" root))
                      (directory (merge-pathnames "*.asd" root)))))
  (dolist (file files)
    (let ((text (uiop:read-file-string file)))
      (uiop:with-output-file (out file :if-exists :supersede)
                             (with-input-from-string (in text)
                               (indentify:indentify in out)))
      (format t "~&formatted  ~A~%" file))))
