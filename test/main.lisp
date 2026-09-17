(defpackage :structural-editing-mcp-tests
  (:use :cl
        :rove
        :structural-editing-mcp.tree
        :structural-editing-mcp.edit
        :structural-editing-mcp.parser)
  (:export))

(in-package :structural-editing-mcp-tests)

(deftest test-string-to-sexp
  (ok (equal '(:path () :file
               (:path (0) :paren
                 (:path (0 0) :leaf a)
                 (:path (0 1) :leaf b)
                 (:path (0 2) :leaf c)))
             (string-to-sexp "(a b c)")))
  (ok (equal '(:path () :file
               (:path (0) :square
                 (:path (0 0) :leaf a)
                 (:path (0 1) :leaf b)
                 (:path (0 2) :leaf c)))
             (string-to-sexp "[a b c]")))
  (ok (equal '(:path () :file
               (:path (0) :curly
                 (:path (0 0) :leaf a)
                 (:path (0 1) :leaf b)
                 (:path (0 2) :leaf c)))
             (string-to-sexp "{a b c}"))))

(deftest test-sexp-to-string
  (ok (string= "(a b c)" (sexp-to-string (string-to-sexp "(a b c)"))))
  (ok (string= "(a b c)" (sexp-to-string '(:paren a b c))))
  (ok (string= "[a b c]" (sexp-to-string (string-to-sexp "[a b c]"))))
  (ok (string= "{a b c}" (sexp-to-string (string-to-sexp "{a b c}")))))

(deftest test-get-node-at-path
  (let ((ast (string-to-sexp "(defn foo [x] (+ x 1))")))
    (ok (equal '(:path (0 2 0) :leaf x) (get-node-at-path ast '(0 2 0))))
    (ok (equal '(:path (0 3) :paren
                 (:path (0 3 0) :leaf +)
                 (:path (0 3 1) :leaf x)
                 (:path (0 3 2) :leaf 1))
               (get-node-at-path ast '(0 3))))))

(deftest test-node-accessors
  (let ((ast (string-to-sexp "(defn foo [x] (+ x 1))")))
    (let ((fn-node (get-node-at-path ast '(0))))
      (ok (equal '(0) (get-node-path fn-node)))
      (ok (eq :paren (get-node-tags fn-node)))
      (ok (= 4 (length (get-node-children fn-node))))
      (multiple-value-bind (path tag children) (parse-node fn-node)
        (ok (equal '(0) path))
        (ok (eq :paren tag))
        (ok (= 4 (length children)))))))

(deftest test-insert-node
  (let ((ast (string-to-sexp "(a c)")))
    (ok (equal (string-to-sexp "(a b c)")
               (insert-node ast '(0) 1 (first (get-node-children (string-to-sexp "b"))))))))

(deftest test-insert-expression
  (let ((ast (string-to-sexp "(a c)")))
    (ok (equal (string-to-sexp "(a b c)")
               (insert-expression ast '(0) 1 "b")))))

(deftest test-overwrite-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a (+ 1 2) c)")
               (overwrite-node ast '(0 1) (first (get-node-children (string-to-sexp "(+ 1 2)"))))))))

(deftest test-overwrite-expression
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a (+ 1 2) c)")
               (overwrite-expression ast '(0 1) "(+ 1 2)")))))

(deftest test-delete-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a c)")
               (delete-node ast '(0 1))))))

(deftest test-pop-node
  (let ((ast (string-to-sexp "(a b c)")))
    (multiple-value-bind (new-tree popped-node) (pop-node ast '(0 1))
      (ok (equal (string-to-sexp "(a c)") new-tree))
      (ok (equal '(:path (0 1) :leaf b) popped-node))))
  (let ((ast (string-to-sexp "(a (b c) d)")))
    (multiple-value-bind (new-tree popped-node) (pop-node ast '(0 1))
      (ok (equal (string-to-sexp "(a d)") new-tree))
      (ok (equal (get-node-at-path (string-to-sexp "(a (b c) d)") '(0 1)) popped-node)))))

(deftest test-copy-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a b c b)")
               (copy-node ast '(0 1) '(0) 3)))))

(deftest test-move-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a c b)")
               (move-node ast '(0 1) '(0) 2)))))

(deftest test-swap-nodes
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a c b)")
               (swap-nodes ast '(0 1) '(0 2))))))

(deftest test-wrap-node
  (let ((ast (string-to-sexp "(a b c)")))
    (ok (equal (string-to-sexp "(a (b) c)")
               (wrap-node ast '(0 1) :paren)))))

(deftest test-wrap-range
  (let ((ast (string-to-sexp "(a b c d)")))
    (ok (equal (string-to-sexp "(a [b c] d)")
               (wrap-range ast '(0) 1 2 :square)))))

(deftest test-unwrap-node
  (let ((ast (string-to-sexp "(a (b c) d)")))
    (ok (equal (string-to-sexp "(a b c d)")
               (unwrap-node ast '(0 1))))))

(deftest test-promote-node
  (let ((ast (string-to-sexp "(a (b c) d)")))
    (ok (equal (string-to-sexp "(b c)")
               (promote-node ast '(0 1))))))

(deftest test-split-node
  (let ((ast (string-to-sexp "(a b c d)")))
    (ok (equal (string-to-sexp "((a b) (c d))")
               (split-node ast '(0) 2)))))

(deftest test-merge-nodes
  (let ((ast (string-to-sexp "((a b) (c d))")))
    (ok (equal (string-to-sexp "((a b c d))")
               (merge-nodes ast '(0 0) '(0 1))))))
