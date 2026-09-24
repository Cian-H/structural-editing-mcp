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
  (:documentation "Root test suite package for structural-editing-mcp."))

(defpackage :structural-editing-mcp-tests/version
  (:use :cl :rove :structural-editing-mcp.version :structural-editing-mcp.mcp)
  (:documentation "Version test suite."))

(defpackage
    :structural-editing-mcp-tests/conditions
  (:use :cl
        :rove
        :structural-editing-mcp.conditions
        :structural-editing-mcp.tree
        :structural-editing-mcp.edit
        :structural-editing-mcp.parser
        :structural-editing-mcp.workspace)
  (:documentation "Conditions and error handling test suite."))

(defpackage :structural-editing-mcp-tests/tree
  (:use :cl
        :rove
        :structural-editing-mcp.conditions
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser)
  (:documentation "AST representation and path indexing test suite."))

(defpackage :structural-editing-mcp-tests/parser
  (:use :cl
        :rove
        :structural-editing-mcp.conditions
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.refactor)
  (:documentation "Parser, printer, and multi-dialect syntax test suite."))

(defpackage :structural-editing-mcp-tests/edit
  (:use :cl
        :rove
        :structural-editing-mcp.conditions
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.edit)
  (:documentation "AST surgery primitives test suite."))

(defpackage
    :structural-editing-mcp-tests/analysis
  (:use :cl
        :rove
        :structural-editing-mcp.conditions
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.analysis
        :structural-editing-mcp.refactor)
  (:documentation "Static analysis, linting, and complexity metrics test suite."))

(defpackage
    :structural-editing-mcp-tests/workspace
  (:use :cl
        :rove
        :structural-editing-mcp.conditions
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.edit
        :structural-editing-mcp.workspace)
  (:documentation "Workspace staging, diffing, and merging test suite."))

(defpackage :structural-editing-mcp-tests/mcp
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
  (:documentation "MCP protocol handler and JSON-RPC dispatch test suite."))