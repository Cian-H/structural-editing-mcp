(uiop:define-package :structural-editing-mcp
                     (:use :cl)
                     (:use-reexport :structural-editing-mcp.conditions
                                    :structural-editing-mcp.version
                                    :structural-editing-mcp.tree
                                    :structural-editing-mcp.parser
                                    :structural-editing-mcp.edit
                                    :structural-editing-mcp.workspace
                                    :structural-editing-mcp.mcp
                                    :structural-editing-mcp.cli)
                     (:export :main :+version+ :get-version)
                     (:documentation
                       "Umbrella package for the structural editing library and MCP server."))

(in-package :structural-editing-mcp)

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (if args
      (structural-editing-mcp.cli:run-cli args)
      (structural-editing-mcp.mcp:start-server))))