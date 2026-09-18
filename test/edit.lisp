(in-package :structural-editing-mcp-tests)

(deftest test-insert-node
         (let ((ast (string-to-sexp "(a c)")))
           (ok (equal (string-to-sexp "(a b c)")
                      (insert-node ast '(0) 1 (first (get-node-children (string-to-sexp "b"))))))))

(deftest test-insert-expression
         (let ((ast (string-to-sexp "(a c)")))
           (ok (equal (string-to-sexp "(a b c)") (insert-expression ast '(0) 1 "b")))))

(deftest test-overwrite-node
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(a (+ 1 2) c)")
                      (overwrite-node ast '(0 1)
                                      (first
                                        (get-node-children (string-to-sexp "(+ 1 2)"))))))))

(deftest test-overwrite-expression
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(a (+ 1 2) c)")
                      (overwrite-expression ast '(0 1) "(+ 1 2)")))))

(deftest test-delete-node
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(a c)") (delete-node ast '(0 1))))))

(deftest test-pop-node
         (let ((ast (string-to-sexp "(a b c)")))
           (multiple-value-bind (new-tree popped-node) (pop-node ast '(0 1))
             (ok (equal (string-to-sexp "(a c)") new-tree))
             (ok (equal '(:path (0 1) :leaf b) popped-node))))
         (let ((ast (string-to-sexp "(a (b c) d)")))
           (multiple-value-bind (new-tree popped-node) (pop-node ast '(0 1))
             (ok (equal (string-to-sexp "(a d)") new-tree))
             (ok (equal (get-node-at-path (string-to-sexp "(a (b c) d)") '(0 1))
                        POPPED-NODE)))))

(deftest test-copy-node
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(a b c b)") (copy-node ast '(0 1) '(0) 3)))))

(deftest test-move-node
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(a c b)") (move-node ast '(0 1) '(0) 2)))))

(deftest test-swap-nodes
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(a c b)") (swap-nodes ast '(0 1) '(0 2))))))

(deftest test-wrap-node
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(a (b) c)") (wrap-node ast '(0 1) :paren)))))

(deftest test-wrap-range
         (let ((ast (string-to-sexp "(a b c d)")))
           (ok (equal (string-to-sexp "(a [b c] d)") (wrap-range ast '(0) 1 2 :square)))))

(deftest test-unwrap-node
         (let ((ast (string-to-sexp "(a (b c) d)")))
           (ok (equal (string-to-sexp "(a b c d)") (unwrap-node ast '(0 1))))))

(deftest test-promote-node
         (let ((ast (string-to-sexp "(a (b c) d)")))
           (ok (equal (string-to-sexp "(b c)") (promote-node ast '(0 1))))))

(deftest test-split-node
         (let ((ast (string-to-sexp "(a b c d)")))
           (ok (equal (string-to-sexp "((a b) (c d))") (split-node ast '(0) 2)))))

(deftest test-merge-nodes
         (let ((ast (string-to-sexp "((a b) (c d))")))
           (ok (equal (string-to-sexp "((a b c d))") (merge-nodes ast '(0 0) '(0 1))))))
