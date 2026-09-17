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
             (string-to-sexp "{a b c}")))
  (ok (equal '(:path () :file
               (:path (0) :comment "; hello
")
               (:path (1) :comment "#| block |#"))
             (string-to-sexp "; hello
#| block |#"))))

(deftest test-sexp-to-string
  (ok (string= "(a b c)" (sexp-to-string (string-to-sexp "(a b c)"))))
  (ok (string= "(a b c)" (sexp-to-string '(:paren a b c))))
  (ok (string= "[a b c]" (sexp-to-string (string-to-sexp "[a b c]"))))
  (ok (string= "{a b c}" (sexp-to-string (string-to-sexp "{a b c}"))))
  (ok (string= "()" (sexp-to-string (string-to-sexp "()"))))
  (ok (string= "[]" (sexp-to-string (string-to-sexp "[]"))))
  (ok (string= "{}" (sexp-to-string (string-to-sexp "{}"))))
  (ok (string= "(format () \"foo\")" (sexp-to-string (string-to-sexp "(format () \"foo\")")))))

(deftest test-replace-pattern
  (let* ((code "(defpackage :foo (:use :cl)) (defun test () (bar 1 2))")
         (ast (string-to-sexp code))
         (res (structural-editing-mcp.refactor:replace-pattern ast "(bar ?x ?y)" "(baz ?y ?x)")))
    (ok (search "(baz 2 1)" (sexp-to-string res)))
    (ok (not (search "(bar 1 2)" (sexp-to-string res))))))
