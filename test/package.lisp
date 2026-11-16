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
  (:export #:temp-test-path)
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
  (:import-from :structural-editing-mcp-tests #:temp-test-path)
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
  (:import-from :structural-editing-mcp-tests #:temp-test-path)
  (:documentation "MCP protocol handler and JSON-RPC dispatch test suite."))

(defpackage :structural-editing-mcp-tests/cli
  (:use :cl
        :rove
        :structural-editing-mcp.conditions
        :structural-editing-mcp.version
        :structural-editing-mcp.workspace
        :structural-editing-mcp.mcp
        :structural-editing-mcp.cli)
  (:import-from :structural-editing-mcp-tests #:temp-test-path)
  (:documentation
    "CLI argument parsing, session persistence, and command dispatch test suite."))

(in-package :structural-editing-mcp-tests)

(defun temp-test-path (filename)
  "Return a portable namestring for FILENAME inside the OS temporary directory."
  (namestring (merge-pathnames filename (truename (uiop:temporary-directory)))))