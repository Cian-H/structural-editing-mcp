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

(deftest test-backquote-and-character-literals
  (testing "backquote tokens do not hang the tokenizer"
    (ok (equal '(:path () :file
                 (:path (0) :paren
                   (:path (0 0) :leaf |`|)
                   (:path (0 1) :paren
                     (:path (0 1 0) :leaf a)
                     (:path (0 1 1) :leaf |,B|))))
               (string-to-sexp "(` (a ,b))")))
    (ok (string= "(` (a ,b))" (sexp-to-string (string-to-sexp "(` (a ,b))")))))

  (testing "character literals parse without delimiter interference"
    (let ((code "(char= ch #\\; #\\( #\\) #\\\" #\\\\)"))
      (ok (equal '(:path () :file
                   (:path (0) :paren
                     (:path (0 0) :leaf char=)
                     (:path (0 1) :leaf ch)
                     (:path (0 2) :leaf #\;)
                     (:path (0 3) :leaf #\()
                     (:path (0 4) :leaf #\))
                     (:path (0 5) :leaf #\")
                     (:path (0 6) :leaf #\\)))
                 (string-to-sexp code)))
      (ok (string= "(char= ch #\\; #\\( #\\) #\\\" #\\\\)"
                   (sexp-to-string (string-to-sexp code)))))))

(deftest test-dialect-parsing-and-printing
  (testing "clojure comma as whitespace"
    (let ((ast (string-to-sexp "[a, b, c]" :dialect :clojure)))
      (ok (equal '(:path () :file
                   (:path (0) :square
                     (:path (0 0) :leaf a)
                     (:path (0 1) :leaf b)
                     (:path (0 2) :leaf c)))
                 ast))))

  (testing "clojure set literals"
    (let* ((code "#{:a :b :c}")
           (ast (string-to-sexp code :dialect :clojure)))
      (ok (eq :set (get-node-tag (first (get-node-children ast)))))
      (ok (string= code (sexp-to-string ast :dialect :clojure)))))

  (testing "scheme boolean tokens"
    (let* ((code "(define flag #t)")
           (ast (string-to-sexp code :dialect :scheme)))
      (ok (string= code (sexp-to-string ast :dialect :scheme))))))

