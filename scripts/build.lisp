#!/usr/bin/env -S devenv shell -- sbcl --script
(require 'asdf)

(let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file ql-setup)
    (load ql-setup)))

(let* ((this-file (or *load-truename* *load-pathname*))
       (project-root (if this-file
                       (uiop:pathname-parent-directory-pathname
                         (uiop:pathname-directory-pathname this-file))
                       (uiop:getcwd))))
  (pushnew project-root asdf:*central-registry* :test #'equal)
  (asdf:load-asd (merge-pathnames "structural-editing-mcp.asd" project-root)))

(asdf:load-system :structural-editing-mcp)

(let ((out (or (uiop:getenv "OUTPUT_BINARY")
               (first (uiop:command-line-arguments))
               "semcp"))
      (compress (if (member :sb-core-compression *features*) t nil)))
  (sb-ext:save-lisp-and-die out
                            :executable t
                            :save-runtime-options t
                            :toplevel 'structural-editing-mcp:main
                            :compression compress))
