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
             (ok (= 4 (length (get-node-children fn-node))))
             (multiple-value-bind (path tag children) (parse-node fn-node)
               (ok (equal '(0) path))
               (ok (eq :paren tag))
               (ok (= 4 (length children)))))))

(deftest test-reindex-paths-nested
  (let* ((ast (string-to-sexp "(defn foo [x] (+ x 1))"))
         (reindexed (reindex-paths ast '(1 2))))
    ;; Root file node at '(1 2)
    (ok (equal '(1 2) (get-node-path reindexed)))
    ;; The defn form at '(1 2 0)
    (let ((fn-node (first (get-node-children reindexed))))
      (ok (equal '(1 2 0) (get-node-path fn-node)))
      (ok (eq :paren (get-node-tag fn-node)))
      ;; Vector [x] at '(1 2 0 2)
      (let ((vec-node (third (get-node-children fn-node))))
        (ok (equal '(1 2 0 2) (get-node-path vec-node)))
        (ok (eq :square (get-node-tag vec-node)))
        (ok (equal '(:path (1 2 0 2 0) :leaf x) (first (get-node-children vec-node)))))
      ;; The (+ x 1) form at '(1 2 0 3)
      (let ((plus-node (fourth (get-node-children fn-node))))
        (ok (equal '(1 2 0 3) (get-node-path plus-node)))
        (ok (eq :paren (get-node-tag plus-node)))
        (ok (equal '(:path (1 2 0 3 0) :leaf +) (first (get-node-children plus-node))))))))



