#!/usr/bin/env -S devenv shell -- sbcl --noinform --noprint --disable-debugger --script
(require 'asdf)

;; Direct all load-time messages to stderr so stdout is strictly reserved for JSON-RPC
(let ((*standard-output* *error-output*))
  (let* ((this-file (or *load-truename* *load-pathname*))
         (project-root (if this-file
                         (uiop:pathname-parent-directory-pathname
                           (uiop:pathname-directory-pathname this-file))
                         (uiop:getcwd))))
    (pushnew project-root asdf:*central-registry* :test #'equal)
    (asdf:load-asd (merge-pathnames "structural-editing-mcp.asd" project-root)))
  (asdf:load-system :structural-editing-mcp))

(structural-editing-mcp.mcp:start-server)
