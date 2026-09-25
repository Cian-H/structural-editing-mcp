(in-package :structural-editing-mcp-tests/parser)

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
                   (sexp-to-string (string-to-sexp code))))))

  (testing "space and whitespace character literals"
    (let ((code "(char= ch #\\  #\\Tab #\\Newline)"))
      (ok (equal '(:path () :file
                   (:path (0) :paren
                    (:path (0 0) :leaf char=)
                    (:path (0 1) :leaf ch)
                    (:path (0 2) :leaf #\Space)
                    (:path (0 3) :leaf #\Tab)
                    (:path (0 4) :leaf #\Newline)))
                 (string-to-sexp code)))))

  (testing "unclosed string and block comment error handling"
    (ok (signals (string-to-sexp "\"unclosed string") 'sexp-parse-error))
    (ok (signals (string-to-sexp "#| unclosed block comment")
                 'sexp-parse-error)))

  (testing "escaped strings and unicode handling"
    (let* ((code "\"hello\\nworld\\t\\\"quoted\\\"\"")
           (ast (string-to-sexp code))
           (val (get-node-leaf-value (first (get-node-children ast)))))
      (ok (equal (format nil "hello~Cworld~C\"quoted\"" #\Newline #\Tab) val)))
    (let* ((code "\"unicode \\u0041 and \\x42\"")
           (ast (string-to-sexp code))
           (val (get-node-leaf-value (first (get-node-children ast)))))
      (ok (equal "unicode A and B" val)))
    (let* ((code "\"braced \\u{0043}\"")
           (ast (string-to-sexp code))
           (val (get-node-leaf-value (first (get-node-children ast)))))
      (ok (equal "braced C" val))))

  (testing "fennel multiline string literals"
    (let* ((code "[[hello world]]")
           (ast (string-to-sexp code :dialect :fennel))
           (val (get-node-leaf-value (first (get-node-children ast)))))
      (ok (equal "hello world" val)))))

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

(deftest test-community-formatting-rules
  (testing "defpackage formats clauses with 2-space indentation and aligned exports"
    (let* ((code "(defpackage :my-pkg
  (:use :cl)
  (:export :symbol-one-with-a-longer-name-to-test
           :symbol-two-with-a-longer-name-to-test))")
           (ast (string-to-sexp code))
           (printed (sexp-to-string ast)))
      (ok (search "(:use :cl)" printed))
      (ok (search "  (:export :symbol-one-with-a-longer-name-to-test" printed))
      (ok (search "           :symbol-two-with-a-longer-name-to-test))" printed))))

  (testing "define-condition formats name on line 1, slots and options indented 2 spaces"
    (let* ((code "(define-condition my-error (error)
  ((msg :initarg :msg :reader error-msg))
  (:report (lambda (c s)
             (format s \"~A\" c))))")
           (ast (string-to-sexp code))
           (printed (sexp-to-string ast)))
      (ok (search "(define-condition my-error (error)" printed))
      (ok (search "  ((msg :initarg :msg" printed))
      (ok (search "  (:report" printed))
      (ok (search "(lambda (c s)" printed))))

  (testing "lambda keeps parameter list on line 1 and indents body 2 spaces"
    (let* ((code "(lambda (condition stream)
  (format stream \"Error: ~A\" condition))")
           (ast (string-to-sexp code))
           (printed (sexp-to-string ast)))
      (ok (search "(lambda (condition stream)" printed))
      (ok (search "  (format stream" printed))))

  (testing "if indents branches 2 spaces"
    (let* ((code "(if (valid-p x)
    (process x)
    (handle-error x))")
           (ast (string-to-sexp code))
           (printed (sexp-to-string ast)))
      (ok (search "(if (valid-p x)" printed))
      (ok (search "  (process x)" printed))
      (ok (search "  (handle-error x))" printed))))

  (testing "when indents body 2 spaces"
    (let* ((code "(when (ready-p)
  (step-one)
  (step-two))")
           (ast (string-to-sexp code))
           (printed (sexp-to-string ast)))
      (ok (search "(when (ready-p)" printed))
      (ok (search "  (step-one)" printed))
      (ok (search "  (step-two))" printed))))

  (testing "multiple-value-bind aligns value form under binding list and body 2 spaces"
    (let* ((code "(multiple-value-bind (a b)
    (compute-values)
  (use a)
  (use b))")
           (ast (string-to-sexp code))
           (printed (sexp-to-string ast)))
      (ok (search "(multiple-value-bind (a b)" printed))
      (ok (search "                     (compute-values)" printed))
      (ok (search "  (use a)" printed))))

  (testing "general function call aligns subsequent arguments under first argument"
    (let* ((code "(format stream \"Very long message template string exceeding eighty characters: ~A\" k v)")
           (ast (string-to-sexp code))
           (printed (sexp-to-string ast)))
      (ok (search "(format stream" printed))
      (ok (search "        \"Very long message" printed))
      (ok (search "        k" printed))
      (ok (search "        v)" printed)))))

(deftest test-toplevel-source-preservation
  (testing "string-to-sexp returns original source slices for each top-level form"
    (let* ((code ";;; Header comment
(defun foo (x)
  \"My special docstring with    spaces.\"
  (+ x 1))

;; Middle comment
(defun bar (y)
  (* y 2))")
           (ast nil)
           (slices nil))
      (multiple-value-setq (ast slices) (string-to-sexp code))
      (ok (= 4 (length slices)))
      (ok (string= ";;; Header comment" (string-trim '(#\Newline #\Space) (aref slices 0))))
      (ok (search "\"My special docstring with    spaces.\"" (aref slices 1)))
      (ok (search "(* y 2)" (aref slices 3)))
      (let ((output (with-output-to-string (s)
                      (structural-editing-mcp.parser:print-file-with-clean-sources ast ast slices s))))
        (ok (string= code output))))))

(deftest test-reader-macro-roundtrip
  (testing "clojure reader macros round-trip exactly"
    ;; #(...) anonymous functions and ' quote must print back as
    ;; glued tokens (#(...) and '(...), not "# (" / "' (").
    (let* ((code "(map #(inc %1) '(1 2 3))")
           (ast (string-to-sexp code :dialect :clojure)))
      (ok (string= code (sexp-to-string ast :dialect :clojure))))))


(deftest test-clos-dialect-formatter-protocol
  (testing
      "dialect-supports-indentify-p method dispatch"
    (let
        ((node (string-to-sexp "(defun foo () (+ 1 2))")))
      (ok (dialect-supports-indentify-p :common-lisp node))
      (ok (dialect-supports-indentify-p :emacs-lisp node))
      (ok (dialect-supports-indentify-p :scheme node))
      (ok (not (dialect-supports-indentify-p :clojure node)))
      (ok (not (dialect-supports-indentify-p :fennel node)))))
  (testing "format-dialect-form method dispatch"
    (let ((node (string-to-sexp "(fn [x] (+ x 1))")))
      (let
          ((fnl-str (with-output-to-string (s) (format-dialect-form :fennel node s 0)))
           (cl-str (with-output-to-string (s) (format-dialect-form :common-lisp node s 0))))
        (ok (stringp fnl-str))
        (ok (stringp cl-str))
        (ok (search "fn" fnl-str)))))
  (testing "collection-delimiters method dispatch"
    (multiple-value-bind (open close)
                         (collection-delimiters :common-lisp :paren)
      (ok (string= open "("))
      (ok (string= close ")")))
    (multiple-value-bind (open close)
                         (collection-delimiters :clojure :square)
      (ok (string= open "["))
      (ok (string= close "]")))))