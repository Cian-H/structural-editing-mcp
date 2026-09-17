(defpackage :structural-editing-mcp-tests
  (:use :cl
        :rove
        :structural-editing-mcp.parser
        :structural-editing-mcp.paredit)
  (:export))

(in-package :structural-editing-mcp-tests)

(deftest test-string-to-sexp
  (ok (equal '(:paren a b c) (string-to-sexp "(a b c)")))
  (ok (equal '(:square a b c) (string-to-sexp "[a b c]")))
  (ok (equal '(:curly a b c) (string-to-sexp "{a b c}"))))

(deftest test-sexp-to-string
  (ok (string= "(a b c)" (sexp-to-string '(:paren a b c))))
  (ok (string= "(a b c)" (sexp-to-string '(a b c))))
  (ok (string= "[a b c]" (sexp-to-string '(:square a b c))))
  (ok (string= "{a b c}" (sexp-to-string '(:curly a b c)))))

(deftest test-slurp-right
  (ok (equal '((a b) c) (slurp-right '((a) b c)))))

(deftest test-barf-right
  (ok (equal '((a) b c) (barf-right '((a b) c)))))

(deftest test-slurp-left
  (ok (equal '(a (b c)) (slurp-left '(a (b) c)))))

(deftest test-barf-left
  (ok (equal '(a (b) c) (barf-left '(a (b c))))))

(deftest test-wrap
  (ok (equal '(wrapper a) (wrap '(a) 'wrapper))))

(deftest test-convolute
  (ok (equal '(b (a c)) (convolute '(a (b c))))))

(deftest test-splice
  (ok (equal '(a b c) (splice '(a (b c))))))

(deftest test-raise
  (ok (equal '(b) (raise '(a (b) c)))))

(deftest test-split
  (ok (equal '((hello) (world)) (split '(hello world)))))

(deftest test-join
  (ok (equal '(hello world) (join '((hello) (world))))))
