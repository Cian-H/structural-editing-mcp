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

(defun find-target-suite-or-test (name)
  "Find a test suite package or a specific test symbol matching NAME."
  (let* ((upname (string-upcase name))
         (normalized-name (if (uiop:string-prefix-p "TEST-" upname)
                            (subseq upname 5)
                            upname))
         (pkg-candidates (list (find-symbol upname :keyword)
                               (find-symbol (format nil "STRUCTURAL-EDITING-MCP-TESTS/~A" normalized-name) :keyword)
                               (format nil "STRUCTURAL-EDITING-MCP-TESTS/~A" normalized-name)
                               (format nil "STRUCTURAL-EDITING-MCP-TESTS/~A" upname)
                               (format nil "STRUCTURAL-EDITING-MCP-TESTS.~A" normalized-name)
                               (format nil "STRUCTURAL-EDITING-MCP-TESTS.~A" upname)))
         (pkg (loop for cand in pkg-candidates
                    thereis (and cand (find-package cand)))))
    (if pkg
      (cons :suite pkg)
      ;; Search for symbol in all structural-editing-mcp-tests packages
      (loop for p in (list-all-packages)
            thereis (when (search "STRUCTURAL-EDITING-MCP-TESTS" (package-name p))
                      (let ((sym (find-symbol upname p)))
                        (and sym (cons :test sym))))))))

(let* ((args (uiop:command-line-arguments))
       (style (cond
                ((member "--spec" args :test #'string-equal) :spec)
                ((member "--dot" args :test #'string-equal) :dot)
                (t :dot)))
       (test-names (remove-if (lambda (arg) (member arg '("--spec" "--dot") :test #'string-equal)) args)))
  (setf rove:*default-reporter* style)
  (if test-names
    (let ((all-passed t))
      (dolist (name test-names)
        (let ((target (find-target-suite-or-test name)))
          (cond
            ((and target (eq :suite (car target)))
              (let ((*package* (cdr target)))
                (unless (rove:run (cdr target) :style style)
                  (setf all-passed nil))))
            ((and target (eq :test (car target)))
              (let* ((sym (cdr target))
                     (*package* (symbol-package sym)))
                (unless (rove:run-test sym :style style)
                  (setf all-passed nil))))
            (t
              (format *error-output* "~&Error: Test or suite '~A' not found.~%" name)
              (setf all-passed nil)))))
      (unless all-passed
        (uiop:quit 1)))
    (unless (rove:run :structural-editing-mcp/tests :style style)
      (uiop:quit 1))))

