(in-package :structural-editing-mcp-tests)

(deftest test-conditions
         (signals (string-to-sexp "(a b c") 'sexp-parse-error)
         (signals (string-to-sexp "(a b c]") 'sexp-parse-error)
         (signals (insert-expression (string-to-sexp "(a b c)") '(0) 1 "") 'sexp-parse-error)
         (signals (overwrite-expression (string-to-sexp "(a b c)") '(0 1) "") 'sexp-parse-error)
         (signals (update-node-at-path (string-to-sexp "(a b c)") '(0 99) #'identity) 'invalid-path-error)
         (signals (merge-nodes (string-to-sexp "((a) ((b)))") '(0 0) '(0 1 0)) 'invalid-path-error)
         (let ((*workspace-tree* nil))
           (signals (write-workspace) 'workspace-error)))
