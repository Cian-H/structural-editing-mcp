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

(defun peek-char-ahead (string index len &optional (offset 1))
  "Return character at (+ index offset) in STRING if within bounds [0, len), otherwise NIL."
  (declare (type string string)
           (type fixnum index len offset))
  (let ((target (+ index offset)))
    (when (< target len)
      (char string target))))

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
    ((and (char= (char string index) (code-char 35))
          (eql (peek-char-ahead string index len) (code-char 124)))
     (values (1+ depth) 2))
    ((and (char= (char string index) (code-char 124))
          (eql (peek-char-ahead string index len) (code-char 35)))
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
  (case (char-code ch)
    (40 :paren-open)
    (41 :paren-close)
    (91 :square-open)
    (93 :square-close)
    (123 :curly-open)
    (125 :curly-close)
    (otherwise nil)))

(defun read-default-atom-token (string index len dialect)
  "Read an atom token starting at INDEX. Return (values token-entry next-index)."
  (let* ((start index)
         (next-idx (skip-atom-chars string index len dialect)))
    (if (= start next-idx)
        (values (parse-token string start (1+ start)) (1+ start))
        (values (parse-token string start next-idx) next-idx))))

(defun tokenize-next-token (string index len dialect)
  "Read the next token starting at INDEX in STRING. Return (values token next-index has-tok-p)."
  (let ((ch (char string index)))
    (cond
      ((whitespace-p ch dialect)
       (values nil (1+ index) nil))
      ((char= ch (code-char 59))
       (multiple-value-bind (tok next) (read-line-comment-token string index len)
         (values tok next t)))
      ((and (char= ch (code-char 35)) (eql (peek-char-ahead string index len) (code-char 124)))
       (multiple-value-bind (tok next) (read-block-comment-token string index len)
         (values tok next t)))
      ((and (char= ch (code-char 35)) (eql (peek-char-ahead string index len) #\\))
       (multiple-value-bind (tok next) (read-escaped-char-token string index len dialect)
         (values tok next t)))
      ((and (char= ch (code-char 35)) (eql (peek-char-ahead string index len) (code-char 123)))
       (values '(:delim . :set-open) (+ index 2) t))
      ((delimiter-char-token ch)
       (values (cons :delim (delimiter-char-token ch)) (1+ index) t))
      ((char= ch (code-char 34))
       (multiple-value-bind (tok next) (read-string-literal string index len)
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

(defun print-clause-collection (open close child-strings stream indent)
  "Format collection where first child is a sub-collection (e.g. let bindings or cond clauses)."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let ((clause-indent (make-string (+ indent (length open)) :initial-element #\Space)))
    (dolist (c (rest child-strings))
      (terpri stream)
      (write-string clause-indent stream)
      (write-string c stream)))
  (write-string close stream))

(defun print-def-body-lines (body-cs stream indent-body)
  "Print remaining body forms BODY-CS indented with INDENT-BODY."
  (dolist (c body-cs)
    (terpri stream)
    (write-string indent-body stream)
    (write-string c stream)))

(defun print-def-inline-args (rest-cs stream indent-body)
  "Print name and signature on the same line as DEF keyword."
  (write-char #\Space stream)
  (write-string (first rest-cs) stream)
  (write-char #\Space stream)
  (write-string (second rest-cs) stream)
  (print-def-body-lines (cddr rest-cs) stream indent-body))

(defun print-def-split-args (rest-cs stream indent indent-body)
  "Print name inline and signature indented on next line."
  (write-char #\Space stream)
  (write-string (first rest-cs) stream)
  (let ((indent-arg (make-string (+ indent 4) :initial-element #\Space)))
    (when (rest rest-cs)
      (terpri stream)
      (write-string indent-arg stream)
      (write-string (second rest-cs) stream))
    (print-def-body-lines (cddr rest-cs) stream indent-body)))

(defun print-def-stacked-args (rest-cs stream indent indent-body)
  "Print all arguments stacked on new lines."
  (let ((indent-arg (make-string (+ indent 4) :initial-element #\Space)))
    (loop for c in rest-cs
          for i from 1
          do (terpri stream)
             (write-string (if (<= i 2) indent-arg indent-body) stream)
             (write-string c stream))))

(defun def-inline-args-fit-p (indent first-cs rest-cs first-arg second-arg)
  "Return T if definition name and parameter signature fit on the first line."
  (and (>= (length rest-cs) 2)
       (not (find #\Newline first-arg))
       (not (find #\Newline second-arg))
       (<= (+ indent (length first-cs) (length first-arg) (length second-arg) 4) 80)))

(defun def-split-args-fit-p (indent first-cs rest-cs first-arg)
  "Return T if definition name fits on the first line."
  (and (>= (length rest-cs) 1)
       (not (find #\Newline first-arg))
       (<= (+ indent (length first-cs) (length first-arg) 3) 80)))

(defun print-definition-collection (open close child-strings stream indent)
  "Format definition collection (defun, defmacro, defmethod, etc.)."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let* ((first-cs (first child-strings))
         (rest-cs (rest child-strings))
         (indent-body (make-string (+ indent 2) :initial-element #\Space))
         (first-arg (first rest-cs))
         (second-arg (second rest-cs)))
    (cond
      ((def-inline-args-fit-p indent first-cs rest-cs first-arg second-arg)
       (print-def-inline-args rest-cs stream indent-body))
      ((def-split-args-fit-p indent first-cs rest-cs first-arg)
       (print-def-split-args rest-cs stream indent indent-body))
      (t
       (print-def-stacked-args rest-cs stream indent indent-body))))
  (write-string close stream))

(defun make-indent-string (n)
  "Create an indent string of N spaces."
  (make-string n :initial-element #\Space))

(defun indent-2-spaces (indent)
  "Create an indent string of (+ INDENT 2) spaces."
  (make-indent-string (+ indent 2)))

(defun indent-4-spaces (indent)
  "Create an indent string of (+ INDENT 4) spaces."
  (make-indent-string (+ indent 4)))

(defun write-inline-arg (arg stream)
  "Write ARG prefixed with a space to STREAM if ARG is non-nil."
  (when arg
    (write-char #\Space stream)
    (write-string arg stream)))

(defun write-indented-lines (lines stream indent-str)
  "Print each line in LINES to STREAM prefixed by a newline and INDENT-STR."
  (dolist (c lines)
    (terpri stream)
    (write-string indent-str stream)
    (write-string c stream)))

(defun keyword-token-string-p (str)
  "Return T if STR is formatted as a keyword token (starts with colon)."
  (and (stringp str) (> (length str) 1) (char= (char str 0) #\:)))

(defparameter *operator-category-table*
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (entry '(("DEFPACKAGE" . :def-package)
                     ("DEFCLASS" . :def-type)
                     ("DEFINE-CONDITION" . :def-type)
                     ("DEFSTRUCT" . :def-type)
                     ("DEFTYPE" . :def-type)
                     ("DEFVAR" . :def-var)
                     ("DEFPARAMETER" . :def-var)
                     ("DEFCONSTANT" . :def-var)
                     ("DEFCUSTOM" . :def-var)
                     ("DEFUN" . :def-fn)
                     ("DEFMACRO" . :def-fn)
                     ("DEFMETHOD" . :def-fn)
                     ("DEFGENERIC" . :def-fn)
                     ("DEFN" . :def-fn)
                     ("DEFN-" . :def-fn)
                     ("DEFMACRO*" . :def-fn)
                     ("LET" . :binding)
                     ("LET*" . :binding)
                     ("FLET" . :binding)
                     ("LABELS" . :binding)
                     ("MACROLET" . :binding)
                     ("SYMBOL-MACROLET" . :binding)
                     ("WHEN-LET" . :binding)
                     ("WHEN-LET*" . :binding)
                     ("IF-LET" . :binding)
                     ("IF-LET*" . :binding)
                     ("WHEN-SOME" . :binding)
                     ("IF-SOME" . :binding)
                     ("BINDING" . :binding)
                     ("IF" . :if)
                     ("IF-NOT" . :if)
                     ("WHEN" . :when)
                     ("UNLESS" . :when)
                     ("WHEN-NOT" . :when)
                     ("COND" . :cond)
                     ("CASE" . :case)
                     ("CCASE" . :case)
                     ("ECASE" . :case)
                     ("TYPECASE" . :case)
                     ("CTYPECASE" . :case)
                     ("ETYPECASE" . :case)
                     ("MATCH" . :case)
                     ("MULTIPLE-VALUE-BIND" . :mvb)
                     ("DESTRUCTURING-BIND" . :mvb)
                     ("MULTIPLE-VALUE-SETQ" . :mvb)
                     ("UNWIND-PROTECT" . :with)
                     ("HANDLER-CASE" . :with)
                     ("HANDLER-BIND" . :with)
                     ("RESTART-CASE" . :with)
                     ("DOLIST" . :iteration)
                     ("DOTIMES" . :iteration)
                     ("LOOP" . :iteration)
                     ("DO" . :iteration)
                     ("DO*" . :iteration)
                     ("LAMBDA" . :lambda)
                     ("FN" . :lambda)))
      (setf (gethash (car entry) ht) (cdr entry)))
    ht)
  "Lookup table mapping operator names to category keywords.")

(defun classify-form-operator (name)
  "Classify operator symbol NAME into a formatting category."
  (when name
    (or (gethash name *operator-category-table*)
        (cond
          ((starts-with-subseq "WITH-" name) :with)
          ((starts-with-subseq "DEF" name) :def-fn)
          (t :general)))))

(defun print-defpackage-collection (open close child-strings stream indent)
  "Format defpackage with package name inline and clauses indented 2 spaces."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (write-inline-arg (second child-strings) stream)
  (write-indented-lines (cddr child-strings) stream (indent-2-spaces indent))
  (write-string close stream))

(defun print-type-collection (open close child-strings stream indent)
  "Format defclass / define-condition / defstruct / deftype."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let* ((rest-cs (rest child-strings))
         (first-arg (first rest-cs))
         (second-arg (second rest-cs))
         (body-indent (indent-2-spaces indent))
         (inline-both-p (and first-arg second-arg
                             (not (find #\Newline first-arg))
                             (not (find #\Newline second-arg))
                             (<= (+ indent (length (first child-strings)) (length first-arg) (length second-arg) 4) 80))))
    (cond
      (inline-both-p
       (write-inline-arg first-arg stream)
       (write-inline-arg second-arg stream)
       (write-indented-lines (cddr rest-cs) stream body-indent))
      (first-arg
       (write-inline-arg first-arg stream)
       (write-indented-lines (rest rest-cs) stream body-indent))
      (t nil)))
  (write-string close stream))

(defun print-defvar-val-doc (val doc stream indent-body inline-p)
  "Print DEFVAR initial value and docstring."
  (when val
    (if inline-p
        (write-inline-arg val stream)
        (progn
          (terpri stream)
          (write-string indent-body stream)
          (write-string val stream))))
  (when doc
    (terpri stream)
    (write-string indent-body stream)
    (write-string doc stream)))

(defun print-defvar-collection (open close child-strings stream indent)
  "Format defvar / defparameter / defconstant."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let* ((rest-cs (rest child-strings))
         (name (first rest-cs))
         (val (second rest-cs))
         (doc (third rest-cs))
         (indent-body (indent-2-spaces indent))
         (inline-p (and val (not (find #\Newline val))
                        (<= (+ indent (length (first child-strings)) (length (or name "")) (length val) 4) 80))))
    (write-inline-arg name stream)
    (print-defvar-val-doc val doc stream indent-body inline-p))
  (write-string close stream))

(defun print-lambda-collection (open close child-strings stream indent)
  "Format lambda / fn with parameters inline on line 1 and body indented 2 spaces."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let ((rest-cs (rest child-strings)))
    (write-inline-arg (first rest-cs) stream)
    (write-indented-lines (rest rest-cs) stream (indent-2-spaces indent)))
  (write-string close stream))

(defun print-if-collection (open close child-strings stream indent)
  "Format if / if-not with then/else branches indented 4 spaces."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let ((rest-cs (rest child-strings)))
    (write-inline-arg (first rest-cs) stream)
    (write-indented-lines (rest rest-cs) stream (indent-4-spaces indent)))
  (write-string close stream))

(defun print-when-collection (open close child-strings stream indent)
  "Format when / unless / iteration with test/spec inline and body indented 2 spaces."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let ((rest-cs (rest child-strings)))
    (write-inline-arg (first rest-cs) stream)
    (write-indented-lines (rest rest-cs) stream (indent-2-spaces indent)))
  (write-string close stream))

(defun print-cond-collection (open close child-strings stream indent)
  "Format cond with each clause on its own line indented 2 spaces."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (write-indented-lines (rest child-strings) stream (indent-2-spaces indent))
  (write-string close stream))

(defun print-case-collection (open close child-strings stream indent)
  "Format case with keyform inline and clauses indented 2 spaces."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let ((rest-cs (rest child-strings)))
    (write-inline-arg (first rest-cs) stream)
    (write-indented-lines (rest rest-cs) stream (indent-2-spaces indent)))
  (write-string close stream))

(defun print-mvb-collection (open close child-strings stream indent)
  "Format multiple-value-bind / destructuring-bind."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let ((rest-cs (rest child-strings)))
    (write-inline-arg (first rest-cs) stream)
    (when (second rest-cs)
      (terpri stream)
      (write-string (indent-4-spaces indent) stream)
      (write-string (second rest-cs) stream))
    (write-indented-lines (cddr rest-cs) stream (indent-2-spaces indent)))
  (write-string close stream))

(defun print-with-collection (open close child-strings stream indent)
  "Format with-* / unwind-protect / handler-case."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let ((rest-cs (rest child-strings)))
    (write-inline-arg (first rest-cs) stream)
    (write-indented-lines (rest rest-cs) stream (indent-2-spaces indent)))
  (write-string close stream))

(defun print-binding-collection (open close child-strings stream indent)
  "Format let / let* / flet / labels / when-let form."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (write-inline-arg (second child-strings) stream)
  (write-indented-lines (cddr child-strings) stream (indent-2-spaces indent))
  (write-string close stream))

(defun write-arg-item (curr stream indent-str rem-cs align-col)
  "Write CURR argument item or keyword-pair to STREAM, returning remaining list."
  (terpri stream)
  (write-string indent-str stream)
  (if (and (keyword-token-string-p curr)
           (second rem-cs)
           (not (find #\Newline (second rem-cs)))
           (<= (+ align-col (length curr) 1 (length (second rem-cs))) 80))
      (progn
        (write-string curr stream)
        (write-char #\Space stream)
        (write-string (second rem-cs) stream)
        (cddr rem-cs))
      (progn
        (write-string curr stream)
        (rest rem-cs))))

(defun print-aligned-arguments (rem-cs stream align-col)
  "Print remaining arguments aligned at ALIGN-COL."
  (let ((indent-str (make-indent-string align-col)))
    (loop while rem-cs do
      (setf rem-cs (write-arg-item (first rem-cs) stream indent-str rem-cs align-col)))))

(defun print-stacked-arguments (rem-cs stream indent)
  "Print remaining arguments indented by 2 spaces."
  (let ((indent-str (indent-2-spaces indent)))
    (loop while rem-cs do
      (setf rem-cs (write-arg-item (first rem-cs) stream indent-str rem-cs (+ indent 2))))))

(defun keyword-arg-pair-p (first-arg second-arg indent open op-len)
  "Return T if first argument is a keyword that can be paired with second argument."
  (and (keyword-token-string-p first-arg)
       second-arg
       (not (find #\Newline first-arg))
       (not (find #\Newline second-arg))
       (<= (+ indent (length open) op-len 1 (length first-arg) 1 (length second-arg)) 80)))

(defun can-inline-first-arg-p (first-arg op-len indent open first-pair-p)
  "Return T if first argument can be placed on the first line."
  (and first-arg
       (<= op-len 18)
       (or first-pair-p
           (and (not (find #\Newline first-arg))
                (<= (+ indent (length open) op-len 1 (length first-arg)) 80)))))

(defun print-general-inlined (first-arg second-arg first-pair-p rest-cs stream indent open op-len)
  "Print inlined first argument and aligned subsequent arguments."
  (write-char #\Space stream)
  (write-string first-arg stream)
  (let ((rem-cs (rest rest-cs)))
    (when first-pair-p
      (write-char #\Space stream)
      (write-string second-arg stream)
      (setf rem-cs (cddr rest-cs)))
    (print-aligned-arguments rem-cs stream (+ indent (length open) op-len 1))))

(defun print-general-collection (open close child-strings stream indent)
  "Format general function application or expression collection with Riastradh argument alignment and keyword pairing."
  (write-string open stream)
  (write-string (first child-strings) stream)
  (let* ((op-len (length (first child-strings)))
         (rest-cs (rest child-strings))
         (first-arg (first rest-cs))
         (second-arg (second rest-cs))
         (first-pair-p (keyword-arg-pair-p first-arg second-arg indent open op-len))
         (can-inline (can-inline-first-arg-p first-arg op-len indent open first-pair-p)))
    (cond
      ((null rest-cs) nil)
      (can-inline
       (print-general-inlined first-arg second-arg first-pair-p rest-cs stream indent open op-len))
      (t
       (print-stacked-arguments rest-cs stream indent))))
  (write-string close stream))

(defun dispatch-multiline-collection (op-cat open close children child-strings stream indent)
  "Dispatch multiline collection printing by OP-CAT."
  (declare (ignore children))
  (case op-cat
    (:def-package (print-defpackage-collection open close child-strings stream indent))
    (:def-type    (print-type-collection open close child-strings stream indent))
    (:def-var     (print-defvar-collection open close child-strings stream indent))
    (:def-fn      (print-definition-collection open close child-strings stream indent))
    (:binding     (print-binding-collection open close child-strings stream indent))
    (:if          (print-if-collection open close child-strings stream indent))
    ((:when :iteration) (print-when-collection open close child-strings stream indent))
    (:cond        (print-cond-collection open close child-strings stream indent))
    (:case        (print-case-collection open close child-strings stream indent))
    (:mvb         (print-mvb-collection open close child-strings stream indent))
    (:with        (print-with-collection open close child-strings stream indent))
    (:lambda      (print-lambda-collection open close child-strings stream indent))
    (otherwise    (print-general-collection open close child-strings stream indent))))

(defun print-multiline-collection (open close children child-strings stream indent)
  "Format multiline collection dispatching on operator classification."
  (let* ((first-child (first children))
         (first-tag (get-node-tag first-child)))
    (if (member first-tag '(:paren :square :curly))
        (print-clause-collection open close child-strings stream indent)
        (let* ((first-name (get-node-symbol-name first-child))
               (op-cat (classify-form-operator first-name)))
          (dispatch-multiline-collection op-cat open close children child-strings stream indent)))))

(defun always-multiline-op-p (op-cat children)
  "Return T if OP-CAT should never be formatted as a single line."
  (case op-cat
    ((:def-package :def-type :cond :mvb) t)
    ((:lambda) (>= (length children) 3))
    ((:if) (>= (length children) 4))
    ((:case) (>= (length children) 3))
    ((:when :iteration) (>= (length children) 4))
    ((:binding) (or (>= (length children) 4)
                    (let ((binds (second children)))
                      (and (member (get-node-tag binds) '(:paren :square))
                           (>= (length (get-node-children binds)) 2)))))
    (otherwise nil)))

(defun print-collection (open close children stream indent)
  "Format and print a collection delimited by OPEN and CLOSE to STREAM."
  (if (null children)
      (format stream "~A~A" open close)
      (let* ((child-strings (mapcar (lambda (c) (sexp-to-string c :indent (+ indent 2)))
                                    children))
             (first-child (first children))
             (first-name (get-node-symbol-name first-child))
             (op-cat (classify-form-operator first-name))
             (single-line (format nil "~A~{~A~^ ~}~A" open child-strings close)))
        (if (and (not (always-multiline-op-p op-cat children))
                 (not (find #\Newline single-line))
                 (<= (length single-line) 80))
            (write-string single-line stream)
            (print-multiline-collection open close children child-strings stream indent)))))

(defun print-toplevel-sequence (children stream indent dialect)
  "Print sequence of top-level CHILDREN separated by blank lines."
  (loop for (c . rest) on children do
    (print-sexp c stream indent :dialect dialect)
    (when rest
      (terpri stream)
      (terpri stream))))

(defun print-file-with-clean-sources (file-node clean-node clean-sources stream &optional (dialect *current-dialect*))
  "Print FILE-NODE to STREAM, emitting original source text from CLEAN-SOURCES for unmodified forms."
  (let* ((current-children (get-node-children file-node))
         (clean-children (and clean-node (get-node-children clean-node))))
    (loop for (c . rest) on current-children do
      (let ((clean-pos (and clean-children (position c clean-children :test #'equal))))
        (if (and clean-pos clean-sources (< clean-pos (length clean-sources)))
            (write-string (aref clean-sources clean-pos) stream)
            (progn
              (print-sexp c stream 0 :dialect dialect)
              (when rest
                (terpri stream)
                (terpri stream))))))))

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
       (print-toplevel-sequence children stream indent dialect))
      ((node _ (or :workspace 'workspace) children)
       (print-toplevel-sequence children stream indent dialect))
      ((guard (node _ tag children)
              (supported-dialect-p tag))
       (print-toplevel-sequence children stream indent tag))
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
