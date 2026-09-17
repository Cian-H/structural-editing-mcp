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
