(uiop:define-package :structural-editing-mcp.version
  (:use :cl)
  (:export :+version+
           :get-version)
  (:documentation "Version information for structural-editing-mcp."))

(in-package :structural-editing-mcp.version)

(defparameter +version+
  #.(or (let* ((this-file (or *compile-file-truename* *load-truename* *load-pathname*))
               (dir (if this-file
                        (uiop:pathname-parent-directory-pathname
                         (uiop:pathname-directory-pathname this-file))
                        (uiop:getcwd)))
               (file (merge-pathnames "version.txt" dir)))
          (when (probe-file file)
            (string-trim '(#\Space #\Tab #\Newline #\Return)
                         (uiop:read-file-line file))))
        (when (find-package :asdf)
          (let ((sys (uiop:symbol-call :asdf :find-system :structural-editing-mcp nil)))
            (when sys (uiop:symbol-call :asdf :component-version sys))))
        "2026.9.18")
  "The current CalVer version string of structural-editing-mcp.")

(defun get-version ()
  "Return the current CalVer version string."
  +version+)
