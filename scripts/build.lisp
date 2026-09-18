#!/usr/bin/env -S devenv shell -- sbcl --script
(require 'asdf)

(let* ((this-file (or *load-truename* *load-pathname*))
       (project-root (if this-file
                       (uiop:pathname-parent-directory-pathname
                         (uiop:pathname-directory-pathname this-file))
                       (uiop:getcwd))))
  ;; Automatically synchronize version.txt before building
  (let ((calver-script (merge-pathnames "scripts/calver.lisp" project-root)))
    (when (probe-file calver-script)
      (ignore-errors
        (uiop:run-program (list (namestring calver-script) "--update")
                          :output *standard-output*
                          :error-output *error-output*))))
  (pushnew project-root asdf:*central-registry* :test #'equal)
  (asdf:load-asd (merge-pathnames "structural-editing-mcp.asd" project-root)))

(asdf:load-system :structural-editing-mcp)

(sb-ext:save-lisp-and-die "semcp"
                          :executable t
                          :save-runtime-options t
                          :toplevel 'structural-editing-mcp:main
                          :compression t)
