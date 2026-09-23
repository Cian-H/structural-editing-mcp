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

;; --- Edge cases: overlapping/dependent paths ---------------------------------

(deftest test-swap-nodes-different-parents
         ;; Swap where the two nodes have different parents and different depths,
         ;; exercising the "overwrite the deeper path first" branch of swap-nodes.
         (let ((ast (string-to-sexp "(a b (c d))")))
           (ok (equal (string-to-sexp "(c b (a d))")
                      (swap-nodes ast '(0 0) '(0 2 0)))))
         (let ((ast (string-to-sexp "(a (b c) d)")))
           (ok (equal (string-to-sexp "(a (b d) c)")
                      (swap-nodes ast '(0 1 1) '(0 2))))))

(deftest test-swap-nodes-missing-path
         ;; Swapping against a path that does not exist must signal
         ;; invalid-path-error rather than inject a literal NIL into the tree.
         (ok (signals (swap-nodes (string-to-sexp "(a b c)") '(0 1) '(0 99))
                      'invalid-path-error))
         (ok (signals (swap-nodes (string-to-sexp "(a (b c) d)") '(0 1) '(0 3 0 0))
                      'invalid-path-error)))

(deftest test-copy-node-into-descendant
         ;; Copying a node into one of its own descendants must produce a fresh,
         ;; well-formed duplicate. The embedded copy must NOT be the same cons
         ;; object as its source (no structure sharing / aliasing).
         (let* ((ast (string-to-sexp "(a (b c))"))
                (res (copy-node ast '(0 1) '(0 1) 1)))
           (ok (equal (string-to-sexp "(a (b (b c) c))") res))
           (ok (not (eq (get-node-at-path res '(0 1))
                        (get-node-at-path res '(0 1 1))))))
         ;; Sibling copies must not share structure either.
         (let* ((ast (string-to-sexp "(a (b c))"))
                (res (copy-node ast '(0 1) '(0) 1)))
           (ok (equal (string-to-sexp "(a (b c) (b c))") res))
           (ok (not (eq (get-node-at-path res '(0 1))
                        (get-node-at-path res '(0 2)))))))

(deftest test-move-node-into-own-subtree
         ;; Moving a node to be a descendant of itself is ill-defined and must
         ;; signal invalid-path-error (currently it silently corrupts a leaf).
         (ok (signals (move-node (string-to-sexp "(a (b c) d)") '(0 1) '(0 1) 1)
                      'invalid-path-error))
         (ok (signals (move-node (string-to-sexp "(a (b c))") '(0 1) '(0 1 0) 0)
                      'invalid-path-error)))

(deftest test-move-node-reorder
         ;; Reordering within the same parent is legal; target index is the
         ;; final 0-based position in the resulting child list.
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(b c a)") (move-node ast '(0 0) '(0) 2)))
           (ok (equal (string-to-sexp "(c a b)") (move-node ast '(0 2) '(0) 0)))))

(deftest test-merge-nodes-same-path
         ;; Merging a node with itself would duplicate subtrees; it must signal
         ;; invalid-path-error.
         (ok (signals (merge-nodes (string-to-sexp "((a) (b))") '(0 0) '(0 0))
                      'invalid-path-error)))

(deftest test-merge-nodes-non-adjacent
         ;; Merging non-adjacent siblings splices the two and drops what lies
         ;; between them.
         (let ((ast (string-to-sexp "((a) (b) (c) (d))")))
           (ok (equal (string-to-sexp "((a d) (b) (c))")
                      (merge-nodes ast '(0 0) '(0 3))))))

;; --- Edge cases: root paths and boundary indices -----------------------------

(deftest test-root-path-edge-cases
         ;; pop-node at the workspace root removes the whole tree.
         (let ((ast (string-to-sexp "(a b c)")))
           (multiple-value-bind (new-tree popped) (pop-node ast '())
             (ok (null new-tree))
             (ok (equal ast popped))))
         ;; unwrap-node and promote-node on the root are no-ops.
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal ast (unwrap-node ast '())))
           (ok (equal ast (promote-node ast '())))))

(deftest test-range-and-split-boundaries
         ;; wrap-range: single-element range and full-range wrap.
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(a [b] c)") (wrap-range ast '(0) 1 1 :square)))
           (ok (equal (string-to-sexp "([a b c])") (wrap-range ast '(0) 0 2 :square))))
         ;; wrap-range with an out-of-range end index must signal
         ;; invalid-path-error (currently it silently clamps).
         (ok (signals (wrap-range (string-to-sexp "(a b c)") '(0) 1 5 :square)
                      'invalid-path-error))
         ;; split-node: index i splits into left = first i children,
         ;; right = the rest. Index 0 yields an empty left half.
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal (string-to-sexp "(() (a b c))") (split-node ast '(0) 0)))
           (ok (equal (string-to-sexp "((a) (b c))") (split-node ast '(0) 1)))
           (ok (equal (string-to-sexp "((a b) (c))") (split-node ast '(0) 2)))
           (ok (equal (string-to-sexp "((a b c) ())") (split-node ast '(0) 3))))
         (let ((ast (string-to-sexp "(a)")))
           (ok (equal (string-to-sexp "(() (a))") (split-node ast '(0) 0))))
         ;; Splitting beyond the end must signal invalid-path-error.
         (ok (signals (split-node (string-to-sexp "(a b c)") '(0) 5)
                      'invalid-path-error)))

(deftest test-insert-append-and-leaf-behavior
         ;; insert-node beyond the last index appends; index 0 prepends.
         (let ((ast (string-to-sexp "(a b c)"))
               (zz (first (get-node-children (string-to-sexp "zz")))))
           (ok (equal (string-to-sexp "(a b c zz)") (insert-node ast '(0) 10 zz)))
           (ok (equal (string-to-sexp "(zz a b c)") (insert-node ast '(0) 0 zz))))
         ;; unwrap-node on a leaf is a no-op.
         (let ((ast (string-to-sexp "(a b c)")))
           (ok (equal ast (unwrap-node ast '(0 1)))))
         ;; promote-node on a leaf hoists it into its parent's slot.
         (let ((ast (string-to-sexp "(a (b c) d)")))
           (ok (equal (string-to-sexp "(a c d)") (promote-node ast '(0 1 1))))))

(deftest test-delete-pop-invalid-index
         ;; delete-node / pop-node with an out-of-range index must signal
         ;; invalid-path-error, consistent with insert/overwrite. (Currently
         ;; the out-of-range index is silently swallowed by take/drop.)
         (ok (signals (delete-node (string-to-sexp "(a b c)") '(0 99))
                      'invalid-path-error))
         (ok (signals (pop-node (string-to-sexp "(a b c)") '(0 99))
                      'invalid-path-error)))
