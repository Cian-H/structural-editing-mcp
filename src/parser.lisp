(defpackage :structural-editing-mcp.parser
  (:use :cl
        :alexandria
        :trivia
        :structural-editing-mcp.tree
        :structural-editing-mcp.conditions)
  (:export :string-to-sexp
           :sexp-to-string
           :print-sexp
           :format-sexp
           :parse-atom-string
           :format-atom
           :*current-dialect*)
  (:documentation "Lexer, parser, and pretty-printer serializer for s-expressions."))

(in-package :structural-editing-mcp.parser)

(defvar *current-dialect* :common-lisp
  "Current Lisp dialect being parsed or formatted (:common-lisp, :clojure, :scheme, :emacs-lisp, :fennel).")

(declaim (optimize (speed 2) (safety 3)))

(declaim (inline whitespace-p delimiter-p))

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
  (cond
    ((char= char (code-char 40)) t)
    ((char= char (code-char 41)) t)
    ((char= char (code-char 91)) t)
    ((char= char (code-char 93)) t)
    ((char= char (code-char 123)) t)
    ((char= char (code-char 125)) t)
    ((char= char (code-char 59)) t)
    ((char= char (code-char 34)) t)
    (t nil)))

(defun read-string-literal (string index len)
  "Read an escaped string literal starting after the opening quote."
  (declare (type string string)
           (type fixnum index len))
  (let ((out (make-string-output-stream)))
    (incf index)
    (loop while (< index len)
          for ch of-type character = (char string index)
          do (cond
               ((char= ch #\\)
                (incf index)
                (when (< index len)
                  (write-char (char string index) out)
                  (incf index)))
               ((char= ch (code-char 34))
                (incf index)
                (return (values (get-output-stream-string out) index)))
               (t
                (write-char ch out)
                (incf index)))
          finally (error 'sexp-parse-error
                         :token (get-output-stream-string out)
                         :message "Unterminated string literal"))))

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
      ;; Integer: parses directly from the string buffer without subseq
      ((multiple-value-bind (val parsed-end)
           (parse-integer string :start start :end end :junk-allowed t)
         (when (and val (= (the fixnum parsed-end) end))
           val)))
      ;; Character literal (#\...)
      ((and (>= len 2) (char= (char string start) (code-char 35)) (char= (char string (1+ start)) #\\))
       (let* ((*read-eval* nil)
              (tok (subseq string start end))
              (parsed (ignore-errors (read-from-string tok))))
         (if (characterp parsed)
             parsed
             (intern (string-upcase tok)))))
      ;; Float or ratio
      ((let* ((*read-eval* nil)
              (tok (subseq string start end))
              (parsed (ignore-errors (read-from-string tok))))
         (if (numberp parsed)
             parsed
             (intern (string-upcase tok)))))
      ;; Standard symbol
      (t
       (intern (string-upcase (subseq string start end)))))))

(defun parse-atom-string (token)
  "Parse a token string into a keyword, number, or symbol (compatibility wrapper)."
  (parse-token token 0 (length token)))

(defun tokenize (string &key (dialect *current-dialect*))
  "Tokenize a string into a flat list of delimiter keywords and atomic values."
  (declare (type string string))
  (let ((*current-dialect* dialect)
        (index 0)
        (len (length string))
        (tokens '()))
    (declare (type fixnum index len)
             (type list tokens))
    (loop while (< index len)
          for ch of-type character = (char string index)
          do (cond
               ((whitespace-p ch dialect)
                (incf index))
               ((char= ch (code-char 59))
                (let ((start index))
                  (incf index)
                  (loop while (and (< index len) (not (char= (char string index) #\Newline)))
                        do (incf index))
                  (when (and (< index len) (char= (char string index) #\Newline))
                    (incf index))
                  (push (list :comment (subseq string start index)) tokens)))
               ((and (char= ch (code-char 35)) (< (1+ index) len) (char= (char string (1+ index)) (code-char 124)))
                (let ((start index)
                      (depth 1))
                  (incf index 2)
                  (loop while (and (< index len) (> depth 0))
                        do (cond
                             ((and (char= (char string index) (code-char 35))
                                   (< (1+ index) len)
                                   (char= (char string (1+ index)) (code-char 124)))
                              (incf depth)
                              (incf index 2))
                             ((and (char= (char string index) (code-char 124))
                                   (< (1+ index) len)
                                   (char= (char string (1+ index)) (code-char 35)))
                              (decf depth)
                              (incf index 2))
                             (t
                              (incf index))))
                  (when (> depth 0)
                    (error 'sexp-parse-error
                           :token (subseq string start len)
                           :message "Unterminated multiline block comment #|...|#"))
                  (push (list :comment (subseq string start index)) tokens)))
               ((and (char= ch (code-char 35)) (< (1+ index) len) (char= (char string (1+ index)) #\\))
                (let ((start index))
                  (incf index 2)
                  (if (< index len)
                      (if (or (delimiter-p (char string index))
                              (char= (char string index) #\\)
                              (whitespace-p (char string index) dialect))
                          (incf index)
                          (loop while (and (< index len)
                                           (let ((c (char string index)))
                                             (not (or (whitespace-p c dialect) (delimiter-p c)))))
                                 do (incf index))))
                  (push (parse-token string start index) tokens)))
               ((and (char= ch (code-char 35)) (< (1+ index) len) (char= (char string (1+ index)) (code-char 123)))
                (push '(:delim . :set-open) tokens)
                (incf index 2))
               ((char= ch (code-char 40)) (push '(:delim . :paren-open) tokens) (incf index))
               ((char= ch (code-char 41)) (push '(:delim . :paren-close) tokens) (incf index))
               ((char= ch (code-char 91)) (push '(:delim . :square-open) tokens) (incf index))
               ((char= ch (code-char 93)) (push '(:delim . :square-close) tokens) (incf index))
               ((char= ch (code-char 123)) (push '(:delim . :curly-open) tokens) (incf index))
               ((char= ch (code-char 125)) (push '(:delim . :curly-close) tokens) (incf index))
               ((char= ch (code-char 34))
                (multiple-value-bind (str next) (read-string-literal string index len)
                  (push str tokens)
                  (setf index next)))
               (t
                (let ((start index))
                  (loop while (and (< index len)
                                   (let ((c (char string index)))
                                     (not (or (whitespace-p c dialect)
                                              (delimiter-p c)))))
                        do (incf index))
                  (if (= start index)
                      ;; Invariant fallback: consume at least 1 character to guarantee progress
                      (progn
                        (push (parse-token string start (1+ start)) tokens)
                        (incf index))
                      (push (parse-token string start index) tokens))))))
    (nreverse tokens)))

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
Always returns a (:path () :file ...) node representing the parsed file contents."
  (let* ((*current-dialect* dialect)
         (tokens (tokenize string :dialect dialect)))
    (if (null tokens)
        (list :path '() :file)
        (let ((remaining tokens)
              (forms '())
              (idx 0))
          (loop while remaining do
            (multiple-value-bind (form rest) (parse-single-form remaining (list idx))
              (when form
                (push form forms))
              (incf idx)
              (setf remaining rest)))
          (list* :path '() :file (nreverse forms))))))

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

(defun print-collection (open close children stream indent)
  "Format and print a collection delimited by OPEN and CLOSE to STREAM."
  (if (null children)
      (format stream "~A~A" open close)
      (let* ((child-strings (mapcar (lambda (c) (sexp-to-string c :indent (+ indent 2)))
                                    children))
             (single-line (format nil "~A~{~A~^ ~}~A" open child-strings close)))
        (if (and (not (find #\Newline single-line))
                 (<= (length single-line) 80))
            (write-string single-line stream)
            (let* ((first-child (first children))
                   (first-tag (get-node-tag first-child))
                   (first-name (get-node-symbol-name first-child)))
              (cond
                ;; Case 1: First child is a sub-collection (e.g. let bindings ((a 1) (b 2)) or cond clauses)
                ((member first-tag '(:paren :square :curly))
                 (write-string open stream)
                 (write-string (first child-strings) stream)
                 (let ((clause-indent (make-string (+ indent (length open)) :initial-element #\Space)))
                   (dolist (c (rest child-strings))
                     (terpri stream)
                     (write-string clause-indent stream)
                     (write-string c stream)))
                 (write-string close stream))

                ;; Case 2: Definition form (defun, defmacro, defmethod, etc.)
                ((and first-name (starts-with-subseq "DEF" first-name))
                 (write-string open stream)
                 (write-string (first child-strings) stream)
                 (let ((rest-cs (rest child-strings))
                       (indent-body (make-string (+ indent 2) :initial-element #\Space)))
                   (cond
                     ((and (>= (length rest-cs) 2)
                           (not (find #\Newline (first rest-cs)))
                           (not (find #\Newline (second rest-cs)))
                           (<= (+ indent (length (first child-strings)) (length (first rest-cs)) (length (second rest-cs)) 4) 80))
                      (write-char #\Space stream)
                      (write-string (first rest-cs) stream)
                      (write-char #\Space stream)
                      (write-string (second rest-cs) stream)
                      (dolist (c (cddr rest-cs))
                        (terpri stream)
                        (write-string indent-body stream)
                        (write-string c stream)))
                     ((and (>= (length rest-cs) 1)
                           (not (find #\Newline (first rest-cs)))
                           (<= (+ indent (length (first child-strings)) (length (first rest-cs)) 3) 80))
                      (write-char #\Space stream)
                      (write-string (first rest-cs) stream)
                      (let ((indent-arg (make-string (+ indent 4) :initial-element #\Space)))
                        (when (rest rest-cs)
                          (terpri stream)
                          (write-string indent-arg stream)
                          (write-string (second rest-cs) stream))
                        (dolist (c (cddr rest-cs))
                          (terpri stream)
                          (write-string indent-body stream)
                          (write-string c stream))))
                     (t
                      (let ((indent-arg (make-string (+ indent 4) :initial-element #\Space)))
                        (loop for c in rest-cs
                              for i from 1
                              do (terpri stream)
                                 (write-string (if (<= i 2) indent-arg indent-body) stream)
                                 (write-string c stream))))))
                 (write-string close stream))

                ;; Case 3: let / let* / flet / labels / cond / match
                ((and first-name (or (string= first-name "LET")
                                     (string= first-name "LET*")
                                     (string= first-name "FLET")
                                     (string= first-name "LABELS")
                                     (string= first-name "MACROLET")
                                     (string= first-name "COND")
                                     (string= first-name "MATCH")))
                 (write-string open stream)
                 (write-string (first child-strings) stream)
                 (let ((indent-body (make-string (+ indent 2) :initial-element #\Space))
                       (indent-bind (make-string (+ indent 2) :initial-element #\Space)))
                   (loop for c in (rest child-strings)
                         for i from 1
                         do (terpri stream)
                            (write-string (if (and (not (string= first-name "COND"))
                                                   (not (string= first-name "MATCH"))
                                                   (= i 1))
                                              indent-bind
                                              indent-body)
                                          stream)
                            (write-string c stream)))
                 (write-string close stream))

                ;; Case 4: General function application
                (t
                 (write-string open stream)
                 (write-string (first child-strings) stream)
                 (let* ((first-len (length (first child-strings)))
                        (natural-arg-indent (+ indent first-len 2))
                        (arg-indent (if (<= (+ (length open) first-len 1) 18)
                                        natural-arg-indent
                                        (+ indent 2)))
                        (indent-str (make-string arg-indent :initial-element #\Space)))
                   (loop for c in (rest child-strings)
                         do (terpri stream)
                            (write-string indent-str stream)
                            (write-string c stream)))
                 (write-string close stream))))))))

(defun print-sexp (expr stream &optional (indent 0) &key (dialect *current-dialect*))
  "Serialize EXPR directly to STREAM with proper formatting."
  (declare (type fixnum indent))
  (let ((*current-dialect* dialect))
    (match expr
      ;; Tagged leaf node: (:path _ :leaf val)
      ((leaf _ val)
       (write-atom val stream))
      ((structural-editing-mcp.tree::comment _ text)
       (write-string text stream))
      ((node _ (or :file 'file) children)
       (loop for (c . rest) on children do
         (print-sexp c stream indent :dialect dialect)
         (when rest
           (terpri stream)
           (terpri stream))))
      ((node _ (or :workspace 'workspace) children)
       (loop for (c . rest) on children do
         (print-sexp c stream indent :dialect dialect)
         (when rest
           (terpri stream)
           (terpri stream))))
      ((guard (node _ tag children)
              (member tag '(:common-lisp :clojure :scheme :emacs-lisp :fennel)))
       (let ((*current-dialect* tag))
         (loop for (c . rest) on children do
           (print-sexp c stream indent :dialect tag)
           (when rest
             (terpri stream)
             (terpri stream)))))
      ((node _ (or :paren 'paren) children)
       (print-collection "(" ")" children stream indent))
      ((node _ (or :square 'square) children)
       (print-collection "[" "]" children stream indent))
      ((node _ (or :curly 'curly) children)
       (print-collection "{" "}" children stream indent))
      ((node _ (or :set 'set) children)
       (print-collection "#{" "}" children stream indent))
      ;; Tagged leaf node without :leaf: (:path _ val)
      ((list :path _ val)
       (write-atom val stream))
      ;; Backward-compatible untagged collections:
      ((list* (or :paren 'paren) children)
       (print-collection "(" ")" children stream indent))
      ((list* (or :square 'square) children)
       (print-collection "[" "]" children stream indent))
      ((list* (or :curly 'curly) children)
       (print-collection "{" "}" children stream indent))
      ((list* (or :set 'set) children)
       (print-collection "#{" "}" children stream indent))
      ((list* _ _)
       (print-collection "(" ")" expr stream indent))
      ;; Direct atoms:
      (_
       (write-atom expr stream)))))

(defun sexp-to-string (expr &key (indent 0) (dialect *current-dialect*))
  "Serialize an s-expression back into its string representation with proper formatting."
  (with-output-to-string (out)
    (print-sexp expr out indent :dialect dialect)))

(defun format-sexp (expr indent)
  "Serialize EXPR with INDENT (compatibility wrapper)."
  (sexp-to-string expr :indent indent))
