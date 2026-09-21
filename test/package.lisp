(defpackage :structural-editing-mcp-tests
  (:use :cl
        :rove
        :structural-editing-mcp.conditions
        :structural-editing-mcp.version
        :structural-editing-mcp.tree
        :structural-editing-mcp.edit
        :structural-editing-mcp.parser
        :structural-editing-mcp.analysis
        :structural-editing-mcp.refactor
        :structural-editing-mcp.workspace
        :structural-editing-mcp.mcp)
  (:export)
  (:documentation "Test suite package for structural-editing-mcp."))
