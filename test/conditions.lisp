(in-package :structural-editing-mcp-tests)

(deftest test-conditions
  (signals (string-to-sexp "(a b c") 'sexp-parse-error)
  (signals (string-to-sexp "(a b c]") 'sexp-parse-error)
  (signals (merge-nodes (string-to-sexp "((a) ((b)))") '(0 0) '(0 1 0)) 'invalid-path-error)
  (let ((*workspace-tree* nil))
    (signals (write-workspace) 'workspace-error)))
