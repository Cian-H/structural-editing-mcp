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
