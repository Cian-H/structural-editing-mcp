(defpackage :structural-editing-mcp-tests
  (:use :cl
        :rove
        :structural-editing-mcp.tree
        :structural-editing-mcp.edit
        :structural-editing-mcp.parser)
  (:export))

(in-package :structural-editing-mcp-tests)

(deftest test-string-to-sexp
  (ok (equal '(:path () :paren
               (:path (0) :leaf a)
               (:path (1) :leaf b)
               (:path (2) :leaf c))
             (string-to-sexp "(a b c)")))
  (ok (equal '(:path () :square
               (:path (0) :leaf a)
               (:path (1) :leaf b)
               (:path (2) :leaf c))
             (string-to-sexp "[a b c]")))
  (ok (equal '(:path () :curly
               (:path (0) :leaf a)
               (:path (1) :leaf b)
               (:path (2) :leaf c))
             (string-to-sexp "{a b c}"))))

(deftest test-sexp-to-string
  (ok (string= "(a b c)" (sexp-to-string (string-to-sexp "(a b c)"))))
  (ok (string= "(a b c)" (sexp-to-string '(a b c))))
  (ok (string= "[a b c]" (sexp-to-string (string-to-sexp "[a b c]"))))
  (ok (string= "{a b c}" (sexp-to-string (string-to-sexp "{a b c}")))))

(deftest test-get-node-at-path
  (let ((ast (string-to-sexp "(defn foo [x] (+ x 1))")))
    (ok (equal '(:path (2 0) :leaf x) (get-node-at-path ast '(2 0))))
    (ok (equal '(:path (3) :paren
                 (:path (3 0) :leaf +)
                 (:path (3 1) :leaf x)
                 (:path (3 2) :leaf 1))
               (get-node-at-path ast '(3))))))

(deftest test-insert-node
  (let ((ast (string-to-sexp "(a c)")))
    (ok (equal (string-to-sexp "(a b c)")
               (insert-node ast '() 1 "b")))))

(deftest test-overwrite-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a (+ 1 2) c)")
               (overwrite-node ast '(1) "(+ 1 2)")))))

(deftest test-delete-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a c)")
               (delete-node ast '(1))))))

(deftest test-copy-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a b c b)")
               (copy-node ast '(1) '() 3)))))

(deftest test-move-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a c b)")
               (move-node ast '(1) '() 2)))))

(deftest test-swap-nodes
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a c b)")
               (swap-nodes ast '(1) '(2))))))

(deftest test-wrap-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a (b) c)")
               (wrap-node ast '(1) :paren)))))

(deftest test-wrap-range
  (let ((ast (string-to-sexp "(a b c d)")))
    (ok (equal (string-to-sexp "(a [b c] d)")
               (wrap-range ast '() 1 2 :square)))))

(deftest test-unwrap-node
  (let ((ast (string-to-sexp "(a (b c) d)")))
    (ok (equal (string-to-sexp "(a b c d)")
               (unwrap-node ast '(1))))))

(deftest test-promote-node
  (let ((ast (string-to-sexp "(a (b c) d)")))
    (ok (equal (string-to-sexp "(b c)")
               (promote-node ast '(1))))))

(deftest test-split-node
  (let ((ast (string-to-sexp "(a b c d)")))
    (ok (equal (string-to-sexp "((a b) (c d))")
               (split-node ast '() 2)))))

(deftest test-merge-nodes
  (let ((ast (string-to-sexp "((a b) (c d))")))
    (ok (equal (string-to-sexp "((a b c d))")
               (merge-nodes ast '(0) '(1))))))
