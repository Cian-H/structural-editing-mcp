(in-package :structural-editing-mcp-tests)

(deftest test-search-ast
  (let ((ast (string-to-sexp "(defun compute-total (items) (+ (calculate-subtotal items) (tax items)))")))
    (testing "case-insensitive substring search"
      (let ((paths (search-ast ast "subtotal")))
        (ok (= (length paths) 1))
        (ok (equal paths '((0 3 1 0))))))
    (testing "exact search"
      (let ((exact-paths (search-ast ast "items" :exact t)))
        (ok (= (length exact-paths) 3)))
      (let ((no-match (search-ast ast "item" :exact t)))
        (ok (null no-match))))
    (testing "search under specific path"
      (let ((sub-paths (search-ast ast "items" :path '(0 3 2))))
        (ok (= (length sub-paths) 1))
        (ok (equal sub-paths '((0 3 2 1))))))))

(deftest test-pattern-matching-and-search
  (testing "variable-node-p identification"
    (let ((var-ast (first (get-node-children (string-to-sexp "?x"))))
          (non-var-ast (first (get-node-children (string-to-sexp "x")))))
      (ok (variable-node-p var-ast))
      (ok (not (variable-node-p non-var-ast)))))

  (testing "match-pattern basic bindings"
    (let ((pat (first (get-node-children (string-to-sexp "(foo ?x ?y)"))))
          (target (first (get-node-children (string-to-sexp "(foo 10 (+ 1 2))")))))
      (multiple-value-bind (matched bindings) (match-pattern pat target nil)
        (ok matched)
        (ok (= (length bindings) 2))
        (let ((inst (instantiate-pattern
                     (first (get-node-children (string-to-sexp "(bar ?y ?x)")))
                     bindings)))
          (ok (string= (sexp-to-string inst) "(bar (+ 1 2) 10)"))))))

  (testing "find-pattern-matches across tree"
    (let ((ast (string-to-sexp "(defun foo () (+ 1 2) (bar (+ 3 4)))")))
      (let ((matches (find-pattern-matches ast "(+ ?a ?b)")))
        (ok (= (length matches) 2))
        (ok (equal (mapcar (lambda (m) (getf m :path)) matches)
                   '((0 3) (0 4 1))))))))

(deftest test-lint-if-rules
  (testing "if-progn-to-when detection"
    (let* ((ast (string-to-sexp "(if (> x 0) (progn (step-one) (step-two)))"))
           (findings (lint-ast ast)))
      (ok (= (length findings) 1))
      (let ((f (first findings)))
        (ok (eq (lint-finding-rule f) :if-progn-to-when))
        (ok (equal (lint-finding-path f) '(0)))
        (ok (search "when" (lint-finding-suggested-fix f)))
        (ok (string= (lint-finding-suggested-fix f)
                     "(when (> x 0) (step-one) (step-two))")))))

  (testing "if-nil-to-when detection"
    (let* ((ast (string-to-sexp "(if valid (execute-action) nil)"))
           (findings (lint-ast ast)))
      (ok (= (length findings) 1))
      (let ((f (first findings)))
        (ok (eq (lint-finding-rule f) :if-nil-to-when))
        (ok (string= (lint-finding-suggested-fix f)
                     "(when valid (execute-action))")))))

  (testing "if-not-to-unless detection"
    (let* ((ast (string-to-sexp "(if (not ready) (wait) nil)"))
           (findings (lint-ast ast)))
      (ok (= (length findings) 1))
      (let ((f (first findings)))
        (ok (eq (lint-finding-rule f) :if-not-to-unless))
        (ok (string= (lint-finding-suggested-fix f)
                     "(unless ready (wait))")))))

  (testing "invert-if-not detection"
    (let* ((ast (string-to-sexp "(if (not active) (handle-inactive) (handle-active))"))
           (findings (lint-ast ast)))
      (ok (= (length findings) 1))
      (let ((f (first findings)))
        (ok (eq (lint-finding-rule f) :invert-if-not))
        (ok (string= (lint-finding-suggested-fix f)
                     "(if active (handle-active) (handle-inactive))"))))))

(deftest test-lint-structural-rules
  (testing "single-clause-cond detection"
    (let* ((ast (string-to-sexp "(cond ((> score 100) (celebrate) (level-up)))"))
           (findings (lint-ast ast)))
      (ok (= (length findings) 1))
      (let ((f (first findings)))
        (ok (eq (lint-finding-rule f) :single-clause-cond))
        (ok (string= (lint-finding-suggested-fix f)
                     "(when (> score 100) (celebrate) (level-up))")))))

  (testing "if-boolean-redundant detection"
    (let* ((ast (string-to-sexp "(if (check) t nil)"))
           (findings (lint-ast ast)))
      (ok (= (length findings) 1))
      (let ((f (first findings)))
        (ok (eq (lint-finding-rule f) :if-boolean-redundant)))))

  (testing "redundant-progn detection"
    (let* ((ast (string-to-sexp "(progn (do-something))"))
           (findings (lint-ast ast)))
      (ok (= (length findings) 1))
      (let ((f (first findings)))
        (ok (eq (lint-finding-rule f) :redundant-progn))
        (ok (string= (lint-finding-suggested-fix f) "(do-something)")))))

  (testing "nested-let to let* detection"
    (let* ((ast (string-to-sexp "(let ((a 1)) (let ((b (+ a 2))) (* a b)))"))
           (findings (lint-ast ast)))
      (ok (= (length findings) 1))
      (let ((f (first findings)))
        (ok (eq (lint-finding-rule f) :nested-let))
        (ok (search "let*" (lint-finding-suggested-fix f))))))

  (testing "equal-nil-to-null detection"
    (let* ((ast (string-to-sexp "(equal ptr nil)"))
           (findings (lint-ast ast)))
      (ok (= (length findings) 1))
      (let ((f (first findings)))
        (ok (eq (lint-finding-rule f) :equal-nil-to-null))
        (ok (string= (lint-finding-suggested-fix f) "(null ptr)"))))))

(deftest test-lint-filtering-and-formatting
  (let ((ast (string-to-sexp "(progn (if valid (do-it) nil) (progn 42))")))
    (testing "filter rules by ID"
      (let ((filtered (lint-ast ast :rules '("if-nil-to-when"))))
        (ok (= (length filtered) 1))
        (ok (eq (lint-finding-rule (first filtered)) :if-nil-to-when))))
    (testing "format findings report"
      (let* ((findings (lint-ast ast))
             (report (format-lint-findings findings)))
        (ok (search "Found 2 anti-patterns:" report))
        (ok (search "if-nil-to-when" report))
        (ok (search "redundant-progn" report))))))

(deftest test-branch-complexity
  (testing "simple function has base complexity 1"
    (let* ((ast (first (get-node-children (string-to-sexp "(defun simple (x) (+ x 1))")))))
      (ok (= (compute-branch-complexity ast) 1))))

  (testing "branching constructs increase complexity"
    (let* ((code "(defun complex-fn (x y)
                     (if (> x 0)
                         (when (< y 10)
                           (loop for i from 0 to 5 do (foo i)))
                         (cond ((= x 0) (bar))
                               ((= x -1) (baz)))))")
           (ast (first (get-node-children (string-to-sexp code)))))
      ;; Base 1 + if(1) + when(1) + loop(1) + cond(2 clauses) = 6
      (ok (= (compute-branch-complexity ast) 6))))

  (testing "short-circuit operators increase complexity"
    (let* ((code "(defun check-all (a b c d) (and a b c d))")
           (ast (first (get-node-children (string-to-sexp code)))))
      ;; Base 1 + (4 args - 2) decision points = 3 + 1 = 4
      (ok (= (compute-branch-complexity ast) 4)))))

(deftest test-nesting-depth
  (testing "flat expression depth"
    (let ((ast (first (get-node-children (string-to-sexp "(+ 1 2 3)")))))
      (ok (= (compute-nesting-depth ast) 1))))

  (testing "deeply nested expression depth"
    (let ((ast (first (get-node-children (string-to-sexp "(a (b (c (d 1))))")))))
      (ok (= (compute-nesting-depth ast) 4)))))

(deftest test-form-definition-info
  (let ((fn-ast (first (get-node-children (string-to-sexp "(defun calculate-total (x) x)"))))
        (macro-ast (first (get-node-children (string-to-sexp "(defmacro with-lock ((l) &body body) body)"))))
        (other-ast (first (get-node-children (string-to-sexp "(in-package :foo)")))))
    (multiple-value-bind (is-def name kind) (form-definition-info fn-ast)
      (ok is-def)
      (ok (string-equal name "calculate-total"))
      (ok (eq kind :function)))
    (multiple-value-bind (is-def name kind) (form-definition-info macro-ast)
      (ok is-def)
      (ok (string-equal name "with-lock"))
      (ok (eq kind :macro)))
    (multiple-value-bind (is-def name kind) (form-definition-info other-ast)
      (declare (ignore name kind))
      (ok (not is-def)))))

(deftest test-analyze-complexity-and-reporting
  (let* ((code "(defun simple-fn (x) (+ x 1))
                (defun complex-fn (x)
                  (if (> x 0)
                      (if (> x 10)
                          (if (> x 20)
                              (if (> x 30)
                                  (if (> x 40)
                                      (if (> x 50)
                                          (if (> x 60)
                                              (if (> x 70)
                                                  (if (> x 80)
                                                      (if (> x 90) 100 0)))))))))))")
         (ast (string-to-sexp code))
         (all-metrics (analyze-complexity ast))
         (filtered (analyze-complexity ast :min-complexity 5)))
    (testing "all forms analyzed"
      (ok (= (length all-metrics) 2)))
    (testing "filtering by min-complexity"
      (ok (= (length filtered) 1))
      (let ((m (first filtered)))
        (ok (string-equal (complexity-metrics-name m) "complex-fn"))
        (ok (>= (complexity-metrics-cyclomatic-complexity m) 10))
        (ok (>= (complexity-metrics-max-nesting-depth m) 6))
        (ok (> (length (complexity-metrics-recommendations m)) 0))))
    (testing "format complexity report"
      (let ((report (format-complexity-report filtered)))
        (ok (search "complex-fn" report))
        (ok (search "Cyclomatic Complexity" report))
        (ok (search "Recommendations:" report))))))

(deftest test-duplicate-detection
  (testing "canonicalize-subtree exact vs structural"
    (let ((node1 (first (get-node-children (string-to-sexp "(+ (* x 2) 1)"))))
          (node2 (first (get-node-children (string-to-sexp "(+ (* y 2) 1)")))))
      (ok (string/= (canonicalize-subtree node1 :exact t)
                    (canonicalize-subtree node2 :exact t)))
      (ok (string= (canonicalize-subtree node1 :exact nil)
                   (canonicalize-subtree node2 :exact nil)))))

  (testing "find exact duplicates within a function"
    (let* ((code "(defun calculate (a b)
                    (let ((x (+ (* a 2) (* b 3)))
                          (y (+ (* a 2) (* b 3))))
                      (+ x y)))")
           (ast (string-to-sexp code))
           (dups (find-duplicate-subtrees ast :min-nodes 4 :min-depth 2)))
      (ok (>= (length dups) 1))
      (let ((g (first dups)))
        (ok (= (duplicate-group-occurrence-count g) 2))
        (ok (search "(+ (* a 2) (* b 3))" (duplicate-group-code-snippet g)))
        ;; Intra-function recommendation should mention ast_extract_variable
        (ok (search "ast_extract_variable" (duplicate-group-recommendation g))))))

  (testing "find exact duplicates across functions"
    (let* ((code "(defun fn-one (items)
                    (process-batch (filter-active items)))
                  (defun fn-two (items)
                    (save (process-batch (filter-active items))))")
           (ast (string-to-sexp code))
           (dups (find-duplicate-subtrees ast :min-nodes 3 :min-depth 2)))
      (ok (>= (length dups) 1))
      (let ((g (first dups)))
        (ok (= (duplicate-group-occurrence-count g) 2))
        (ok (search "process-batch" (duplicate-group-code-snippet g)))
        ;; Inter-function recommendation should mention ast_extract_function
        (ok (search "ast_extract_function" (duplicate-group-recommendation g))))))

  (testing "format duplicate report"
    (let* ((code "(defun calculate (a b)
                    (let ((x (+ (* a 2) (* b 3)))
                          (y (+ (* a 2) (* b 3))))
                      (+ x y)))")
           (ast (string-to-sexp code))
           (dups (find-duplicate-subtrees ast :min-nodes 4 :min-depth 2))
           (report (format-duplicate-report dups)))
      (ok (search "Duplicate Subtrees Report" report))
      (ok (search "occurrences" report))
      (ok (search "ast_extract_variable" report)))))


