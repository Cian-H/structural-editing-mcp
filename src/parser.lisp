(defpackage :structural-editing-mcp.parser
  (:use :cl
        :alexandria
        :trivia
        :structural-editing-mcp.tree
        :structural-editing-mcp.conditions)
  (:import-from :serapeum :trim-whitespace :dict :string-prefix-p :fmt)
  (:export :string-to-sexp
           :sexp-to-string
           :print-sexp
           :print-file-with-clean-sources
           :classify-form-operator
           :parse-atom-string
           :format-atom
           :*current-dialect*
           :*supported-dialects*
           :supported-dialect-p
           :peek-char-ahead)
  (:documentation "Lexer, parser, and pretty-printer serializer for s-expressions."))

(in-package :structural-editing-mcp.parser)

(defparameter *supported-dialects*
  '(:common-lisp :clojure :scheme :emacs-lisp :fennel)
  "List of supported Lisp dialect keywords.")

(defun supported-dialect-p (tag)
  "Return T if TAG is a supported Lisp dialect keyword."
  (and (member tag *supported-dialects*) t))

(defvar *current-dialect* :common-lisp
                          "Current Lisp dialect being parsed or formatted (:common-lisp, :clojure, :scheme, :emacs-lisp, :fennel).")

(declaim (optimize (speed 2) (safety 3)))

(declaim (inline whitespace-p delimiter-p peek-char-ahead))

(defun whitespace-p (char &optional (dialect *current-dialect*))
  "Return T if CHAR is whitespace (space, tab, newline, return, and comma in Clojure)."
  (declare (type character char))
  (case char
    ((#\Space #\Tab #\Newline #\Return) t)
    (#\, (eq dialect :clojure))
    (otherwise nil)))

(defun delimiter-p (char)
  "Return T if CHAR is a delimiter character."
  (declare (type character char))
  (case char
    ((#\( #\) #\[ #\] #\{ #\} #\; #\") t)
    (otherwise nil)))

(defun peek-char-ahead (string index len &optional (offset 1))
  "Return character at (+ index offset) in STRING if within bounds [0, len), otherwise NIL."
  (declare (type string string)
           (type fixnum index len offset))
  (let ((target (+ index offset)))
    (when (< target len)
      (char string target))))

(defun read-string-literal (string index len &optional (delimiter #\") (dialect *current-dialect*))
  "Read an escaped string literal starting after the opening delimiter."
  (declare (type string string)
           (type fixnum index len)
           (type character delimiter)
           (ignore dialect))
  (let ((out (make-string-output-stream)))
    (incf index)
    (loop while (< index len)
          for ch of-type character = (char string index)
          do (cond
               ((char= ch #\\)
                 (incf index)
                 (if (>= index len)
                     (error 'sexp-parse-error
                            :token (get-output-stream-string out)
                            :message "Unterminated escape sequence in string literal")
                     (let ((esc (char string index)))
                       (cond
                         ((char= esc #\n)
                          (write-char #\Newline out)
                          (incf index))
                         ((char= esc #\t)
                          (write-char #\Tab out)
                          (incf index))
                         ((char= esc #\r)
                          (write-char #\Return out)
                          (incf index))
                         ((char= esc #\b)
                          (write-char (code-char 8) out)
                          (incf index))
                         ((char= esc #\f)
                          (write-char (code-char 12) out)
                          (incf index))
                         ((char= esc #\0)
                          (write-char (code-char 0) out)
                          (incf index))
                         ((char= esc #\\)
                          (write-char #\\ out)
                          (incf index))
                         ((char= esc #\")
                          (write-char #\" out)
                          (incf index))
                         ((char= esc #\')
                          (write-char #\' out)
                          (incf index))
                         ((char= esc #\u)
                          ;; Unicode escape: \u{HEX...} or \uXXXX
                          (cond
                            ((and (< (1+ index) len)
                                  (char= (char string (1+ index)) #\{))
                             (let ((close-pos (position #\} string :start (+ index 2) :end (min len (+ index 10)))))
                               (if (and close-pos (> close-pos (+ index 2))
                                        (every (lambda (c) (digit-char-p c 16))
                                               (subseq string (+ index 2) close-pos)))
                                   (let ((code (parse-integer string :start (+ index 2) :end close-pos :radix 16)))
                                     (write-char (or (code-char code) #\?) out)
                                     (setf index (1+ close-pos)))
                                   (progn
                                     (write-char esc out)
                                     (incf index)))))
                            ((and (<= (+ index 5) len)
                                  (every (lambda (c) (digit-char-p c 16))
                                         (subseq string (1+ index) (+ index 5))))
                             (let ((code (parse-integer string :start (1+ index) :end (+ index 5) :radix 16)))
                               (write-char (or (code-char code) #\?) out)
                               (incf index 5)))
                            (t
                             (write-char esc out)
                             (incf index))))
                         ((char= esc #\U)
                          ;; 8-hex digit Unicode escape: \UXXXXXXXX
                          (if (and (<= (+ index 9) len)
                                   (every (lambda (c) (digit-char-p c 16))
                                          (subseq string (1+ index) (+ index 9))))
                              (let ((code (parse-integer string :start (1+ index) :end (+ index 9) :radix 16)))
                                (write-char (or (code-char code) #\?) out)
                                (incf index 9))
                              (progn
                                (write-char esc out)
                                (incf index))))
                         ((char= esc #\x)
                          ;; Hex escape: \x{HEX...} or \xXX
                          (cond
                            ((and (< (1+ index) len)
                                  (char= (char string (1+ index)) #\{))
                             (let ((close-pos (position #\} string :start (+ index 2) :end (min len (+ index 6)))))
                               (if (and close-pos (> close-pos (+ index 2))
                                        (every (lambda (c) (digit-char-p c 16))
                                               (subseq string (+ index 2) close-pos)))
                                   (let ((code (parse-integer string :start (+ index 2) :end close-pos :radix 16)))
                                     (write-char (or (code-char code) #\?) out)
                                     (setf index (1+ close-pos)))
                                   (progn
                                     (write-char esc out)
                                     (incf index)))))
                            ((and (<= (+ index 3) len)
                                  (every (lambda (c) (digit-char-p c 16))
                                         (subseq string (1+ index) (+ index 3))))
                             (let ((code (parse-integer string :start (1+ index) :end (+ index 3) :radix 16)))
                               (write-char (or (code-char code) #\?) out)
                               (incf index 3)))
                            (t
                             (write-char esc out)
                             (incf index))))
                         (t
                          (write-char esc out)
                          (incf index))))))
               ((char= ch delimiter)
                 (incf index)
                 (return (values (get-output-stream-string out) index)))
               (t
                 (write-char ch out)
                 (incf index)))
          finally (error 'sexp-parse-error
                         :token (get-output-stream-string out)
                         :message "Unterminated string literal"))))

(defun parse-read-literal (tok predicate-fn)
  "Attempt reading TOK with read-eval disabled; return parsed object if satisfying PREDICATE-FN."
  (let* ((*read-eval* nil)
         (parsed (ignore-errors (read-from-string tok))))
    (when (and parsed (funcall predicate-fn parsed))
      parsed)))

(defun parse-character-token (string start end)
  "Parse character token starting with #\\."
  (let ((tok (subseq string start end)))
    (or (parse-read-literal tok #'characterp)
        (intern (string-upcase tok)))))

(defun parse-numeric-token (string start end)
  "Parse integer, float, or ratio token from STRING in range [START, END)."
  (multiple-value-bind (val parsed-end)
                       (parse-integer string :start start :end end :junk-allowed t)
    (if (and val (= (the fixnum parsed-end) end))
      val
      (parse-read-literal (subseq string start end) #'numberp))))

(defun parse-token (string start end)
  "Parse a token delimited by [START, END) in STRING into a keyword, number, or symbol."
  (declare (type string string)
           (type fixnum start end))
  (let ((len (- end start)))
    (declare (type fixnum len))
    (cond
      ;; Keyword (:foo)
      ((and (>= len 2) (char= (char string start) #\:))
        (intern (string-upcase (subseq string (1+ start) end)) :keyword))
      ;; Character literal (#\...)
      ((and (char= (char string start) (code-char 35)) (eql (peek-char-ahead string start end) #\\))
        (parse-character-token string start end))
      ;; Number or symbol
      (t
        (or (parse-numeric-token string start end)
            (intern (string-upcase (subseq string start end))))))))

(defun parse-atom-string (token)
  "Parse a token string into a keyword, number, or symbol (compatibility wrapper)."
  (parse-token token 0 (length token)))

(declaim (inline skip-atom-chars))
(defun skip-atom-chars (string index len &optional (dialect *current-dialect*))
  "Advance and return INDEX past all characters that are neither whitespace nor delimiters."
  (declare (type string string)
           (type fixnum index len))
  (loop while (and (< index len)
                   (let ((c (char string index)))
                     (not (or (whitespace-p c dialect) (delimiter-p c)))))
        do (incf index))
  index)

(defun read-line-comment-token (string index len)
  "Read line comment starting at INDEX until newline. Return (values token-entry next-index)."
  (let ((start index))
    (incf index)
    (loop while (and (< index len) (not (char= (char string index) #\Newline)))
          do (incf index))
    (when (and (< index len) (char= (char string index) #\Newline))
      (incf index))
    (values (list :comment (subseq string start index)) index)))

(defun step-block-comment-depth (string index len depth)
  "Inspect characters at INDEX in STRING and return (values new-depth index-increment)."
  (cond
    ((and (char= (char string index) #\#)
          (eql (peek-char-ahead string index len) #\|))
      (values (1+ depth) 2))
    ((and (char= (char string index) #\|)
          (eql (peek-char-ahead string index len) #\#))
      (values (1- depth) 2))
    (t
      (values depth 1))))

(defun read-block-comment-token (string index len)
  "Read block comment #|...|# starting at INDEX. Return (values token-entry next-index)."
  (let ((start index)
        (depth 1))
    (incf index 2)
    (loop while (and (< index len) (> depth 0))
          do (multiple-value-bind (new-depth inc)
                                  (step-block-comment-depth string index len depth)
               (setf depth new-depth)
               (incf index inc)))
    (when (> depth 0)
      (error 'sexp-parse-error
             :token (subseq string start len)
             :message "Unterminated multiline block comment #|...|#"))
    (values (list :comment (subseq string start index)) index)))

(defun read-escaped-char-token (string index len dialect)
  "Read character literal #\\... starting at INDEX. Return (values token-entry next-index)."
  (let ((start index))
    (incf index 2)
    (when (< index len)
      (if (or (delimiter-p (char string index))
              (char= (char string index) #\\)
              (whitespace-p (char string index) dialect))
        (incf index)
        (setf index (skip-atom-chars string index len dialect))))
    (values (parse-token string start index) index)))

(defun delimiter-char-token (ch)
  "Return delimiter keyword for CH or NIL if CH is not a single-character delimiter."
  (case ch
    (#\( :paren-open)
    (#\) :paren-close)
    (#\[ :square-open)
    (#\] :square-close)
    (#\{ :curly-open)
    (#\} :curly-close)
    (otherwise nil)))

(defun read-default-atom-token (string index len dialect)
  "Read an atom token starting at INDEX. Return (values token-entry next-index)."
  (let* ((start index)
         (next-idx (skip-atom-chars string index len dialect)))
    (if (= start next-idx)
      (values (parse-token string start (1+ start)) (1+ start))
      (values (parse-token string start next-idx) next-idx))))

(defun read-dispatch-macro-token (string index len dialect)
  "Handle tokens starting with # (block comment, character literal, or set open)."
  (let ((next-ch (peek-char-ahead string index len)))
    (case next-ch
      (#\| (read-block-comment-token string index len))
      (#\\ (read-escaped-char-token string index len dialect))
      (#\{ (values '(:delim . :set-open) (+ index 2)))
      (otherwise nil))))

(defun tokenize-next-token (string index len dialect)
  "Read the next token starting at INDEX in STRING. Return (values token next-index has-tok-p)."
  (let ((ch (char string index)))
    (cond
      ((whitespace-p ch dialect)
        (values nil (1+ index) nil))
      ((char= ch #\;)
        (multiple-value-bind (tok next) (read-line-comment-token string index len)
          (values tok next t)))
      ((char= ch #\#)
        (multiple-value-bind (tok next) (read-dispatch-macro-token string index len dialect)
          (if next
            (values tok next t)
            (multiple-value-bind (atok anext) (read-default-atom-token string index len dialect)
              (values atok anext t)))))
      ((and (eq dialect :fennel)
            (char= ch #\[)
            (eql (peek-char-ahead string index len) #\[))
        (let ((close-pos (search "]]" string :start2 (+ index 2))))
          (if close-pos
            (values (subseq string (+ index 2) close-pos) (+ close-pos 2) t)
            (error 'sexp-parse-error
                   :token (subseq string index)
                   :message "Unterminated Fennel multiline string [[...]]"))))
      ((delimiter-char-token ch)
        (values (cons :delim (delimiter-char-token ch)) (1+ index) t))
      ((char= ch #\")
        (multiple-value-bind (tok next) (read-string-literal string index len #\" dialect)
          (values tok next t)))
      (t
        (multiple-value-bind (tok next) (read-default-atom-token string index len dialect)
          (values tok next t))))))

(defun tokenize-with-spans (string &key (dialect *current-dialect*))
  "Tokenize a string and return (values tokens starts ends) where starts and ends are vectors of character offsets."
  (declare (type string string))
  (let ((*current-dialect* dialect)
        (index 0)
        (len (length string))
        (tokens '())
        (starts '())
        (ends '()))
    (declare (type fixnum index len))
    (loop while (< index len)
          do (let ((cur index))
               (multiple-value-bind (tok next has-tok)
                                    (tokenize-next-token string index len dialect)
                 (when has-tok
                   (push tok tokens)
                   (push cur starts)
                   (push next ends))
                 (setf index next))))
    (values (nreverse tokens)
            (coerce (nreverse starts) 'simple-vector)
            (coerce (nreverse ends) 'simple-vector))))

(defun tokenize (string &key (dialect *current-dialect*))
  "Tokenize a string into a flat list of delimiter keywords and atomic values."
  (multiple-value-bind (tokens starts ends)
                       (tokenize-with-spans string :dialect dialect)
    (declare (ignore starts ends))
    tokens))

(defun parse-collection (close-token tokens current-path child-index)
  "Parse sibling forms until CLOSE-TOKEN, returning (values children remaining-tokens)."
  (let ((remaining tokens)
        (children '())
        (idx child-index))
    (loop
      (match remaining
             (nil
               (error 'sexp-parse-error
                      :token close-token
                      :message (format nil "Unexpected end of input: missing ~A" close-token)))
             ((list* (cons :delim (guard tok (eq tok close-token))) rest)
              (return (values (nreverse children) rest)))
             (_
               (let ((child-path (append current-path (list idx))))
                 (multiple-value-bind (child after-child) (parse-single-form remaining child-path)
                   (push child children)
                   (incf idx)
                   (setf remaining after-child))))))))

(defun parse-single-form (tokens path)
  "Parse a single form (leaf or collection) from TOKENS at PATH."
  (match tokens
         (nil (values nil nil))
         ((list* (cons :delim :paren-open) rest)
          (multiple-value-bind (children remaining) (parse-collection :paren-close rest path 0)
            (values (list* :path path :paren children) remaining)))
         ((list* (cons :delim :square-open) rest)
          (multiple-value-bind (children remaining) (parse-collection :square-close rest path 0)
            (values (list* :path path :square children) remaining)))
         ((list* (cons :delim :curly-open) rest)
          (multiple-value-bind (children remaining) (parse-collection :curly-close rest path 0)
            (values (list* :path path :curly children) remaining)))
         ((list* (cons :delim :set-open) rest)
          (multiple-value-bind (children remaining) (parse-collection :curly-close rest path 0)
            (values (list* :path path :set children) remaining)))
         ((list* (cons :delim (or :paren-close :square-close :curly-close)) _)
          (error 'sexp-parse-error
                 :token (car tokens)
                 :message (format nil "Unexpected closing delimiter: ~A" (car tokens))))
         ((list* (list :comment text) rest)
          (values (list :path path :comment text) rest))
         ((list* atom rest)
          (values (list :path path :leaf atom) rest))))

(defun string-to-sexp (string &key (dialect *current-dialect*))
  "Parse a raw string into an s-expression data structure.
Always returns a (:path () :file ...) node representing the parsed file contents.
Returns (values file-node toplevel-sources) where toplevel-sources is a vector of original source strings."
  (let* ((*current-dialect* dialect))
    (multiple-value-bind (tokens starts ends) (tokenize-with-spans string :dialect dialect)
      (if (null tokens)
        (values (list :path '() :file) #())
        (let ((remaining tokens)
              (forms '())
              (tok-spans '())
              (idx 0)
              (tok-idx 0))
          (loop while remaining do
                (let ((start-tok tok-idx))
                  (multiple-value-bind (form rest) (parse-single-form remaining (list idx))
                    (let* ((consumed (- (length remaining) (length rest)))
                           (end-tok (+ start-tok (1- consumed))))
                      (when form
                        (push form forms)
                        (push (cons (aref starts start-tok) (aref ends end-tok)) tok-spans))
                      (incf idx)
                      (incf tok-idx consumed)
                      (setf remaining rest)))))
          (setf forms (nreverse forms))
          (setf tok-spans (nreverse tok-spans))
          (let ((slices (loop for (span . rest-spans) on tok-spans
                              for next-span = (car rest-spans)
                              for s-start = (car span)
                              for s-end = (if next-span (car next-span) (length string))
                              collect (subseq string s-start s-end))))
            (values (list* :path '() :file forms)
                    (coerce slices 'simple-vector))))))))

(defun write-atom (val stream)
  "Write the string representation of atomic VAL to STREAM."
  (match val
         ((type string)
          (format stream "~S" val))
         ((type character)
          (cond
            ((char= val #\Space) (write-string "#\\space" stream))
            ((not (graphic-char-p val))
              (let ((name (char-name val)))
                (if name
                  (format stream "#\\~A" (string-downcase name))
                  (format stream "~S" val))))
            (t (format stream "~S" val))))
         ((type keyword)
          (format stream ":~A" (string-downcase (symbol-name val))))
         ((type symbol)
          (write-string (string-downcase (symbol-name val)) stream))
         ((type number)
          (format stream "~A" val))
         (_
           (format stream "~A" val))))

(defun format-atom (val)
  "Return the formatted string representation of VAL (compatibility wrapper)."
  (with-output-to-string (s)
    (write-atom val s)))

(defun get-node-symbol-name (node)
  (match node
         ((leaf _ sym) (when (symbolp sym) (symbol-name sym)))
         ((list :path _ sym) (when (symbolp sym) (symbol-name sym)))
         (_ nil)))

;;;; ============================================================================
;;;; Formatter (cl-indentify-powered)
;;;; ============================================================================
;;;
;;; Line layout is delegated to cl-indentify (package INDENTIFY) for every
;;; dialect it supports: Common Lisp, Emacs Lisp, and Scheme.  cl-indentify is
;;; an indenter rather than a pretty-printer - it preserves the line breaks it
;;; is handed and only recomputes indentation.  This module therefore decides
;;; *where* lines break (RAW-FORM-TEXT) and cl-indentify decides how deep each
;;; line is indented.  Clojure and Fennel (whose brace collections and reader
;;; macros cl-indentify cannot handle) use a generic fallback formatter.

(defparameter *max-line-column* 80
                                "Maximum length for which a form is kept on a single line.")

(defparameter *inline-column-limit* 50
                                    "Maximum column out to which short leading children stay on the head line.")

(defparameter *cl-indentify-dialects* '(:common-lisp :emacs-lisp :scheme)
                                      "Dialects whose indentation is delegated to cl-indentify.")

(defun make-indent-string (n)
  "Create an indent string of N spaces."
  (make-string n :initial-element #\Space))

(defun collection-delims (tag)
  "Return (values open close) delimiter strings for collection TAG."
  (case tag
    (:paren  (values "(" ")"))
    (:square (values "[" "]"))
    (:curly  (values "{" "}"))
    (:set    (values "#{" "}"))
    (t       (values "(" ")"))))

(defun single-line-p (str)
  "Return T if STR contains no newline characters."
  (not (find #\Newline str)))

(defun node-has-braces-p (node)
  "Return T if NODE's subtree contains a :curly or :set collection."
  (labels ((walk (n)
             (when (consp n)
               (or (member (get-node-tag n) '(:curly :set))
                   (some #'walk (get-node-children n))))))
    (walk node)))

(defun flat-collection-string (open blocks close)
  "Join BLOCKS with single spaces into a single-line collection string, respecting reader macro gluing."
  (with-output-to-string (s)
    (write-string open s)
    (loop for (b . rest) on blocks
          do (write-string b s)
          when rest
          do (let ((next-b (car rest)))
               (unless (member b '("#" "'") :test #'string=)
                 (write-string " " s))))
    (write-string close s)))

(defparameter *operator-category-table*
  (dict
    "DEFPACKAGE" :def-package
    "DEFCLASS" :def-type
    "DEFINE-CONDITION" :def-type
    "DEFSTRUCT" :def-type
    "DEFTYPE" :def-type
    "DEFVAR" :def-var
    "DEFPARAMETER" :def-var
    "DEFCONSTANT" :def-var
    "DEFCUSTOM" :def-var
    "DEFUN" :def-fn
    "DEFMACRO" :def-fn
    "DEFMETHOD" :def-fn
    "DEFGENERIC" :def-fn
    "DEFN" :def-fn
    "DEFN-" :def-fn
    "DEFMACRO*" :def-fn
    "LET" :binding
    "LET*" :binding
    "FLET" :binding
    "LABELS" :binding
    "MACROLET" :binding
    "SYMBOL-MACROLET" :binding
    "WHEN-LET" :binding
    "WHEN-LET*" :binding
    "IF-LET" :binding
    "IF-LET*" :binding
    "WHEN-SOME" :binding
    "IF-SOME" :binding
    "BINDING" :binding
    "IF" :if
    "IF-NOT" :if
    "WHEN" :when
    "UNLESS" :when
    "WHEN-NOT" :when
    "COND" :cond
    "CASE" :case
    "CCASE" :case
    "ECASE" :case
    "TYPECASE" :case
    "CTYPECASE" :case
    "ETYPECASE" :case
    "MATCH" :case
    "MULTIPLE-VALUE-BIND" :mvb
    "DESTRUCTURING-BIND" :mvb
    "MULTIPLE-VALUE-SETQ" :mvb
    "UNWIND-PROTECT" :with
    "HANDLER-CASE" :with
    "HANDLER-BIND" :with
    "RESTART-CASE" :with
    "DOLIST" :iteration
    "DOTIMES" :iteration
    "LOOP" :iteration
    "DO" :iteration
    "DO*" :iteration
    "LAMBDA" :lambda
    "FN" :lambda)
  "Lookup table mapping operator names to category keywords.")

(defun classify-form-operator (name)
  "Classify operator symbol NAME into a formatting category."
  (when name
    (or (gethash name *operator-category-table*)
        (cond
          ((string-prefix-p "WITH-" name) :with)
          ((string-prefix-p "DEF" name) :def-fn)
          (t :general)))))

(defun head-inline-count (op-cat)
  "Number of leading blocks kept with the head on the first line for OP-CAT."
  (case op-cat
    ((:def-fn :def-var :def-type) 3)
    (:cond 1)
    (otherwise 2)))

(defun binding-form-multiline-p (children)
  "Return T if binding form should be formatted across multiple lines."
  (or (>= (length children) 4)
      (let ((binds (second children)))
        (and (member (get-node-tag binds) '(:paren :square))
             (>= (length (get-node-children binds)) 2)))))

(defparameter *multiline-min-child-counts*
  (dict :def-package 0 :def-type 0 :cond 0 :mvb 0
        :lambda 3 :case 3
        :if 4 :when 4 :iteration 4)
  "Minimum child count to force multiline formatting by operator category.")

(defun always-multiline-op-p (op-cat children)
  "Return T if OP-CAT should never be formatted as a single line."
  (if (eq op-cat :binding)
    (binding-form-multiline-p children)
    (let ((min-count (gethash op-cat *multiline-min-child-counts*)))
      (and min-count (>= (length children) min-count)))))

(defun raw-multiline-text (open close blocks op-cat)
  "Compose BLOCKS into a multiline raw string with the head and short leading blocks inline."
  (with-output-to-string (s)
    (write-string open s)
    (write-string (first blocks) s)
    (let* ((inline-n (min (head-inline-count op-cat) (length blocks)))
           (col (+ 1 (length open) (length (first blocks)))))
      (loop for (b . rest) on (rest blocks)
            for i from 1
            do (if (and (< i inline-n)
                        (single-line-p b)
                        (<= (+ col 1 (length b)) *inline-column-limit*))
                 (progn
                   (write-char #\Space s)
                   (write-string b s)
                   (incf col (+ 1 (length b))))
                 (progn
                   (terpri s)
                   (write-string b s)
                   (setf col (length b))))))
    (write-string close s)))

(defun raw-form-text (node)
  "Serialize NODE to a raw string with natural line breaks and no indentation."
  (match node
         ((leaf _ val)
          (format-atom val))
         ((comment _ text)
          (trim-whitespace text))
         ((node _ tag children)
          (multiple-value-bind (open close) (collection-delims tag)
            (if (null children)
              (format nil "~A~A" open close)
              (let* ((blocks (mapcar #'raw-form-text children))
                     (flat (flat-collection-string open blocks close))
                     (op-cat (classify-form-operator (get-node-symbol-name (first children)))))
                (if (and (every #'single-line-p blocks)
                         (<= (length flat) *max-line-column*)
                         (not (always-multiline-op-p op-cat children)))
                  flat
                  (raw-multiline-text open close blocks op-cat))))))
         ((list :path _ val)
          (format-atom val))
         (_
           (format-atom node))))

(defun generic-form-text (node &optional (indent 0))
  "Serialize NODE with 2-space indentation (fallback for dialects cl-indentify cannot handle)."
  (match node
         ((leaf _ val)
          (format-atom val))
         ((comment _ text)
          (trim-whitespace text))
         ((node _ tag children)
          (multiple-value-bind (open close) (collection-delims tag)
            (if (null children)
              (format nil "~A~A" open close)
              (let* ((blocks (mapcar (lambda (c) (generic-form-text c (+ indent 2))) children))
                     (flat (flat-collection-string open blocks close)))
                (if (and (every #'single-line-p blocks)
                         (<= (length flat) *max-line-column*))
                  flat
                  (with-output-to-string (s)
                    (write-string open s)
                    (write-string (first blocks) s)
                    (let ((pad (make-indent-string (+ indent 2))))
                      (loop for b in (rest blocks)
                            do (terpri s)
                            (write-string pad s)
                            (write-string b s)))
                    (write-string close s)))))))
         ((list :path _ val)
          (format-atom val))
         (_
           (format-atom node))))

(defparameter *cl-indentify-initialized* nil
                                         "Whether cl-indentify default templates have been loaded.")

(defun ensure-cl-indentify-init ()
  "Load cl-indentify templates lazily once."
  (unless *cl-indentify-initialized*
    (indentify:initialize-templates)
    (setf *cl-indentify-initialized* t)))

(defun cl-indentify-text (raw-text)
  "Re-indent RAW-TEXT with cl-indentify, returning the indented string.
   Falls back to RAW-TEXT (log message on *error-output*) if cl-indentify signals."
  (ensure-cl-indentify-init)
  (with-input-from-string (in raw-text)
    (with-output-to-string (out)
      (handler-case (indentify:indentify in out)
        (error (e)
          (format *error-output* "~&cl-indentify reindent failed (~A); using raw text.~%" e)
          (write-string raw-text out))))))

(defun shift-indent-text (text base-indent)
  "Prefix every line of TEXT with BASE-INDENT spaces."
  (if (zerop base-indent)
    text
    (with-output-to-string (s)
      (let ((pad (make-indent-string base-indent))
            (start 0))
        (loop for pos = (position #\Newline text :start start)
              do (write-string pad s)
              (write-string text s :start start :end (or pos (length text)))
              if pos do (write-char #\Newline s)
              while pos
              do (setf start (1+ pos)))))))

(defun use-cl-indentify-p (node dialect)
  "Return T if NODE should be formatted with cl-indentify for DIALECT."
  (and (member dialect *cl-indentify-dialects*)
       (not (node-has-braces-p node))))

(defun print-formatted-form (node stream indent)
  "Render NODE to STREAM with cl-indentify, or the generic fallback for brace dialects."
  (let* ((dialect (or *current-dialect* :common-lisp))
         (text (if (use-cl-indentify-p node dialect)
                 (let ((raw (raw-form-text node)))
                   (if (find #\Newline raw)
                     (cl-indentify-text raw)
                     raw))
                 (generic-form-text node indent))))
    (write-string (shift-indent-text text indent) stream)))


(defun print-toplevel-sequence (children stream indent dialect)
  "Print sequence of top-level CHILDREN separated by blank lines."
  (loop for (c . rest) on children do
        (print-sexp c stream indent :dialect dialect)
        (when rest
          (terpri stream)
          (terpri stream))))

(defun clean-source-for-child (child clean-children clean-sources)
  "Return clean source string for CHILD if it exists unmodified in CLEAN-CHILDREN."
  (when (and clean-children clean-sources)
    (let ((pos (position child clean-children :test #'equal)))
      (when (and pos (< pos (length clean-sources)))
        (aref clean-sources pos)))))

(defun print-file-with-clean-sources (file-node clean-node clean-sources stream &optional (dialect *current-dialect*))
  "Print FILE-NODE to STREAM, emitting original source text from CLEAN-SOURCES for unmodified forms."
  (let* ((current-children (get-node-children file-node))
         (clean-children (and clean-node (get-node-children clean-node))))
    (loop for (c . rest) on current-children do
          (let ((clean-str (clean-source-for-child c clean-children clean-sources)))
            (if clean-str
              (write-string clean-str stream)
              (progn
                (print-sexp c stream 0 :dialect dialect)
                (when rest
                  (terpri stream)
                  (terpri stream))))))))

(defun toplevel-container-tag-p (tag)
  "Return T if TAG represents a top-level container sequence."
  (or (member tag '(:file file :workspace workspace))
      (supported-dialect-p tag)))

(defun print-toplevel-node (expr stream indent dialect)
  "Print top-level container sequence for EXPR."
  (match expr
         ((node _ tag children)
          (let ((d (if (supported-dialect-p tag) tag dialect)))
            (print-toplevel-sequence children stream indent d)))))

(defun collection-tag-p (tag)
  "Return T if TAG is a collection delimiter tag."
  (member tag '(:paren :square :curly :set paren square curly set)))

(defun print-collection-expr (expr stream indent)
  "Render collection EXPR using formatting rules."
  (let ((tag (get-node-tag expr)))
    (cond
      ((collection-tag-p tag)
        (print-formatted-form expr stream indent))
      ((and (consp expr) (collection-tag-p (first expr)))
        (print-formatted-form `(:path nil ,(first expr) ,@(rest expr)) stream indent))
      (t
        (print-formatted-form `(:path nil :paren ,@expr) stream indent)))))

(defun print-leaf-expr (expr stream)
  "Serialize atomic or leaf node EXPR to STREAM."
  (match expr
         ((leaf _ val)
          (write-atom val stream))
         ((structural-editing-mcp.tree::comment _ text)
          (write-string (trim-whitespace text) stream))
         ((list :path _ val)
          (write-atom val stream))
         (_
           (write-atom expr stream))))

(defun print-sexp (expr stream &optional (indent 0) &key (dialect *current-dialect*))
  "Serialize EXPR directly to STREAM with proper formatting."
  (declare (type fixnum indent))
  (let ((*current-dialect* dialect))
    (cond
      ((toplevel-container-tag-p (get-node-tag expr))
        (print-toplevel-node expr stream indent dialect))
      ((or (member (get-node-tag expr) '(:paren :square :curly :set 'paren 'square 'curly 'set))
           (and (consp expr) (not (eq (first expr) :path))))
        (print-collection-expr expr stream indent))
      (t
        (print-leaf-expr expr stream)))))

(defun sexp-to-string (expr &key (indent 0) (dialect *current-dialect*))
  "Serialize an s-expression back into its string representation with proper formatting."
  (with-output-to-string (out)
    (print-sexp expr out indent :dialect dialect)))

