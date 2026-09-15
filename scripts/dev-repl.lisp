#!/usr/bin/env -S devenv shell -- sbcl --script
(require 'asdf)

(let* ((this-file (or *load-truename* *load-pathname*))
       (project-root (if this-file
                         (uiop:pathname-parent-directory-pathname
                          (uiop:pathname-directory-pathname this-file))
                         (uiop:getcwd))))
  (pushnew project-root asdf:*central-registry* :test #'equal)
  (asdf:load-asd (merge-pathnames "structural-editing-mcp.asd" project-root)))
(asdf:load-system :isocline-repl)
(asdf:load-system :structural-editing-mcp)
(asdf:load-system :structural-editing-mcp/tests)
(in-package :structural-editing-mcp)
#+sbcl (sb-ext:enable-debugger)
(isocline-repl:main)
