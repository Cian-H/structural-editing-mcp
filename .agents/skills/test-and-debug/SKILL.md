---
name: test-and-debug
description: Instructions for running, writing, and debugging Rove unit and integration tests for structural-editing-mcp.
---

# Test and Debug Skill

This skill explains how to run the Rove test suite, target specific test suites, write new tests, and debug failures in `structural-editing-mcp`.

---

## 1. Running Tests

### Run Full Test Suite
```bash
./scripts/run-tests.lisp
```
This script loads the ASDF system `structural-editing-mcp/tests` and executes all defined tests across all test modules. It exits with code `0` on success and `1` on failure.

### Run Targeted Tests
You can specify one or more test symbol names as command-line arguments to run only those suites:
```bash
# Run a specific test suite
./scripts/run-tests.lisp test-mcp-tools-list

# Run multiple test suites
./scripts/run-tests.lisp test-tree test-edit test-parser
```

Available test suites (defined in `test/*.lisp` under the package `:structural-editing-mcp-tests`):
- `test-conditions` (`test/conditions.lisp`)
- `test-tree` (`test/tree.lisp`)
- `test-parser` (`test/parser.lisp`)
- `test-edit` (`test/edit.lisp`)
- `test-analysis` (`test/analysis.lisp`)
- `test-binding-analysis-common-lisp` (`test/analysis.lisp`)
- `test-binding-analysis-clojure` (`test/analysis.lisp`)
- `test-binding-analysis-scheme-and-reporting` (`test/analysis.lisp`)
- `test-suggest-refactorings` (`test/analysis.lisp`)
- `test-workspace` (`test/workspace.lisp`)
- `test-workspace-directory-scanning` (`test/workspace.lisp`)
- `test-multi-dialect-workspace` (`test/workspace.lisp`)
- `test-workspace-lifecycle` (`test/workspace.lisp`)
- `test-workspace-merge-and-diff` (`test/workspace.lisp`)
- `test-mcp-initialize` (`test/mcp.lisp`)
- `test-mcp-tools-list` (`test/mcp.lisp`)
- `test-mcp-ast-operations` (`test/mcp.lisp`)
- `test-mcp-workspace-lifecycle` (`test/mcp.lisp`)
- `test-mcp-workspace-status-diff-merge` (`test/mcp.lisp`)

---

## 2. Writing New Tests

Tests use the [Rove](https://github.com/fukamachi/rove) framework.

### Example Test Form
```lisp
(in-package :structural-editing-mcp-tests)

(deftest test-new-feature
  (testing "handles normal inputs"
    (let ((result (my-new-function "input")))
      (ok result "result should be non-nil")
      (ok (string= result "expected") "result matches expected string")))

  (testing "signals condition on invalid path"
    (signals (my-new-function "invalid")
             'structural-editing-mcp.conditions:invalid-path-error)))
```

### Adding New Test Files
1. Create `test/<module>.lisp`.
2. Register the component in `structural-editing-mcp.asd` under the `structural-editing-mcp/tests` system.
3. Add any necessary symbols to `test/package.lisp`.

---

## 3. Debugging Failures

When a test fails:
1. Run only the failing test suite for faster feedback:
   ```bash
   ./scripts/run-tests.lisp <failing-test-name>
   ```
2. If an unexpected condition is thrown, inspect the backtrace. If running interactively, start SBCL with:
   ```bash
   devenv shell -- sbcl
   ```
   Then in the REPL:
   ```lisp
   (asdf:load-system :structural-editing-mcp/tests)
   (rove:run-test 'structural-editing-mcp-tests:failing-test-name)
   ```
3. Never bypass test failures by disabling assertions; fix the root cause in the AST transform or parser logic.
