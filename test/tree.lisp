(in-package :structural-editing-mcp-tests)

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
      (ok (eq :paren (get-node-tag fn-node)))
      (ok (eq :paren (get-node-tags fn-node)))
      (ok (= 4 (length (get-node-children fn-node))))
      (multiple-value-bind (path tag children) (parse-node fn-node)
        (ok (equal '(0) path))
        (ok (eq :paren tag))
        (ok (= 4 (length children)))))))
