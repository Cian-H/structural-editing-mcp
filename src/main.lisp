(uiop:define-package :structural-editing-mcp
  (:use :cl)
  (:use-reexport :structural-editing-mcp.conditions
                 :structural-editing-mcp.utils
                 :structural-editing-mcp.tree
                 :structural-editing-mcp.parser
                 :structural-editing-mcp.edit
                 :structural-editing-mcp.workspace)
  (:documentation "Umbrella package for the structural editing library and MCP server."))

(in-package :structural-editing-mcp)

