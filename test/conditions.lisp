(in-package :structural-editing-mcp-tests/conditions)

(deftest test-conditions
         (testing
           "sexp-parse-error on malformed delimiters"
           (ok (signals (string-to-sexp "(a b c") 'sexp-parse-error))
           (ok (signals (string-to-sexp "(a b c]") 'sexp-parse-error)))
         (testing
           "sexp-parse-error on invalid insertion or overwrite input"
           (ok
             (signals
               (insert-expression (string-to-sexp "(a b c)") '(0) 1 "")
               'sexp-parse-error))
           (ok
             (signals
               (overwrite-expression (string-to-sexp "(a b c)") '(0 1) "")
               'sexp-parse-error)))
         (testing
           "invalid-path-error on out-of-bounds or non-existent paths"
           (ok
             (signals
               (update-node-at-path (string-to-sexp "(a b c)") '(0 99) #'identity)
               'invalid-path-error))
           (ok
             (signals
               (merge-nodes (string-to-sexp "((a) ((b)))") '(0 0) '(0 1 0))
               'invalid-path-error)))
         (testing
           "workspace-error when committing uninitialized workspace"
           (let* ((ctx (make-workspace-context)))
             (setf (structural-editing-mcp.workspace::workspace-context-tree ctx) nil)
             (with-workspace-context (ctx) (ok (signals (write-workspace) 'workspace-error))))))