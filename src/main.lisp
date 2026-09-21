(uiop:define-package
  :structural-editing-mcp
  (:use :cl)
  (:use-reexport
    :structural-editing-mcp.conditions
    :structural-editing-mcp.version
    :structural-editing-mcp.tree
    :structural-editing-mcp.parser
    :structural-editing-mcp.edit
    :structural-editing-mcp.workspace
    :structural-editing-mcp.mcp)
  (:export :main
           :+version+
           :get-version)
  (:documentation
    "Umbrella package for the structural editing library and MCP server."))

(in-package :structural-editing-mcp)

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (cond
      ((or (member "--version" args :test #'string=)
           (member "-v" args :test #'string=))
        (format t "structural-editing-mcp ~A~%" +version+)
        (uiop:quit 0))
      ((or (member "--help" args :test #'string=)
           (member "-h" args :test #'string=))
        (format t "Usage: semcp [options]~%~%")
        (format t "Options:~%")
        (format t "  -v, --version    Print version and exit~%")
        (format t "  -h, --help       Print this help message and exit~%~%")
        (format t "Starts the Model Context Protocol (MCP) server communicating via standard input/output.~%")
        (uiop:quit 0))
      (t
        (structural-editing-mcp.mcp:start-server)))))