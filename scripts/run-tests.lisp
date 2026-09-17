#!/usr/bin/env -S devenv shell -- sbcl --script
(require 'asdf)

(let* ((this-file (or *load-truename* *load-pathname*))
       (project-root (if this-file
                         (uiop:pathname-parent-directory-pathname
                          (uiop:pathname-directory-pathname this-file))
                         (uiop:getcwd))))
  (pushnew project-root asdf:*central-registry* :test #'equal)
  (asdf:load-asd (merge-pathnames "structural-editing-mcp.asd" project-root)))

(asdf:load-system :structural-editing-mcp/tests)

(let ((test-names (uiop:command-line-arguments)))
  (if test-names
      (let ((all-passed t))
        (dolist (name test-names)
          (let ((sym (find-symbol (string-upcase name) :structural-editing-mcp-tests)))
            (if sym
                (let ((*package* (find-package :structural-editing-mcp-tests)))
                  (unless (rove:run-test sym)
                    (setf all-passed nil)))
                (progn
                  (format *error-output* "~&Error: Test ~A not found in :structural-editing-mcp-tests~%" name)
                  (setf all-passed nil)))))
        (unless all-passed
          (uiop:quit 1)))
      (unless (rove:run :structural-editing-mcp/tests)
        (uiop:quit 1))))
