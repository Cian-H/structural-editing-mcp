(in-package :structural-editing-mcp-tests)

(deftest test-utils
         (ok (equal '(a b c) (insert-at '(a c) 1 'b)))
         (ok (equal '(b c) (insert-at '(c) 0 'b)))
         (ok (equal '(a c) (remove-at '(a b c) 1)))
         (multiple-value-bind (before after) (split-at 2 '(a b c d))
           (ok (equal '(a b) before))
           (ok (equal '(c d) after))))
