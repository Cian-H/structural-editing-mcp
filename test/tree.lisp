(in-package :structural-editing-mcp-tests/tree)

(deftest test-get-node-at-path
  (testing
      "retrieves leaf and compound nodes by AST path"
    (let
        ((ast (string-to-sexp "(defn foo [x] (+ x 1))")))
      (ok (equal '(:path (0 2 0) :leaf x) (get-node-at-path ast '(0 2 0))))
      (ok
        (equal '
          (:path (0 3)
                 :paren
                 (:path (0 3 0) :leaf +)
                 (:path (0 3 1) :leaf x)
                 (:path (0 3 2) :leaf 1))
          (get-node-at-path ast '(0 3)))))))

(deftest test-node-accessors
  (testing
      "accessors parse and inspect AST node components"
    (let
        ((ast (string-to-sexp "(defn foo [x] (+ x 1))")))
      (let ((fn-node (get-node-at-path ast '(0))))
        (ok (equal '(0) (get-node-path fn-node)))
        (ok (eq :paren (get-node-tag fn-node)))
        (ok (= 4 (length (get-node-children fn-node))))
        (multiple-value-bind (path tag children)
                             (parse-node fn-node)
          (ok (equal '(0) path))
          (ok (eq :paren tag))
          (ok (= 4 (length children))))))))

(deftest test-reindex-paths-nested
  (testing
      "reindexes AST paths accurately across nested structures"
    (let*
        ((ast (string-to-sexp "(defn foo [x] (+ x 1))"))
         (reindexed (reindex-paths ast '(1 2))))
      ;; Root file node at '(1 2)
      (ok (equal '(1 2) (get-node-path reindexed)))
      ;; The defn form at '(1 2 0)
      (let
          ((fn-node (first (get-node-children reindexed))))
        (ok (equal '(1 2 0) (get-node-path fn-node)))
        (ok (eq :paren (get-node-tag fn-node)))
        ;; Vector [x] at '(1 2 0 2)
        (let
            ((vec-node (third (get-node-children fn-node))))
          (ok (equal '(1 2 0 2) (get-node-path vec-node)))
          (ok (eq :square (get-node-tag vec-node)))
          (ok (equal '(:path (1 2 0 2 0) :leaf x) (first (get-node-children vec-node)))))
        ;; The (+ x 1) form at '(1 2 0 3)
        (let
            ((plus-node (fourth (get-node-children fn-node))))
          (ok (equal '(1 2 0 3) (get-node-path plus-node)))
          (ok (eq :paren (get-node-tag plus-node)))
          (ok (equal '(:path (1 2 0 3 0) :leaf +) (first (get-node-children plus-node)))))))))

(deftest test-node-ids-and-handles
  (testing
      "assigns and preserves monotonic Node IDs (#n<id>) across reindexing"
    (let*
        ((ast (string-to-sexp "(defun foo (x) (+ x 1))"))
         (fn-node (get-node-at-path ast '(0)))
         (id (ensure-node-id fn-node))
         (handle (get-node-id-handle fn-node)))
      (ok (integerp id))
      (ok (stringp handle))
      (ok (and (> (length handle) 2) (string= "#n" (subseq handle 0 2))))
      ;; Reindex tree to new path '(0 0)
      (let*
          ((reindexed (reindex-paths ast '(0 0)))
           (new-fn (get-node-at-path reindexed '(0 0 0)))
           (new-handle (get-node-id-handle new-fn)))
        ;; Node handle must be preserved across reindexing
        (ok (string= handle new-handle))
        ;; Handle resolution returns updated path
        (ok (equal '(0 0 0) (resolve-symbolic-path reindexed handle)))
        (ok (equal '(0 0 0) (get-node-path (get-node-at-path reindexed handle))))))))

(deftest test-symbolic-path-selectors
  (testing
      "resolves symbolic form and operator selectors like 'defun:target-fn'"
    (let*
        ((ast (string-to-sexp "(defun first-fn () 1) (defun target-fn (a b) (+ a b))")))
      (ok (equal '(1) (resolve-symbolic-path ast '("defun:target-fn"))))
      (ok (equal '(1) (resolve-symbolic-path ast '("target-fn"))))
      (ok (equal '(1 3 0) (resolve-symbolic-path ast '("defun:target-fn" 3 0))))
      (let
          ((leaf-node (get-node-at-path ast '("defun:target-fn" 3 0))))
        (ok (eq :leaf (get-node-tag leaf-node)))
        (ok (equal '(:path (1 3 0) :leaf +) leaf-node))))))