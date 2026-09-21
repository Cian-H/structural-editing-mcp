(uiop:define-package :structural-editing-mcp.version
                     (:use :cl)
                     (:import-from :serapeum :eval-always)
                     (:export :+version+
                              :get-version)
                     (:documentation "Version information for structural-editing-mcp."))

(in-package :structural-editing-mcp.version)

(eval-always
  (defun resolve-version ()
    "Resolve the system version from ASDF component metadata or version.txt."
    (or (ignore-errors
          (asdf:component-version (asdf:find-system :structural-editing-mcp nil)))
        (ignore-errors
          (string-trim '(#\Space #\Tab #\Newline #\Return)
                       (uiop:read-file-line
                         (asdf:system-relative-pathname :structural-editing-mcp "version.txt"))))
        "2026.9.18")))

(defparameter +version+ #.(resolve-version)
                        "The current CalVer version string of structural-editing-mcp.")

(defun get-version ()
  "Return the current CalVer version string."
  +version+)
