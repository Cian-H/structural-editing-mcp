(defpackage :structural-editing-mcp.parser
  (:use :cl
        :trivia
        :structural-editing-mcp.tree
        :structural-editing-mcp.conditions)
  (:export :string-to-sexp
           :sexp-to-string)
  (:documentation "Lexer, parser, and pretty-printer serializer for s-expressions."))

(in-package :structural-editing-mcp.parser)

(declaim (inline whitespace-p delimiter-p))

(defun whitespace-p (char)
  "Return T if CHAR is whitespace (space, tab, newline, return, comma)."
  (case char
    ((#\Space #\Tab #\Newline #\Return #\,) t)
    (otherwise nil)))

(defun delimiter-p (char)
  "Return T if CHAR is a delimiter character."
  (case char
    ((#\( #\) #\[ #\] #\{ #\} #\; #\") t)
    (otherwise nil)))

(defun read-string-literal (string index len)
  "Read an escaped string literal starting after the opening quote."
  (let ((out (make-string-output-stream)))
    (incf index)
    (loop while (< index len)
          for ch = (char string index)
          do (cond
               ((char= ch #\\)
                (incf index)
                (when (< index len)
                  (write-char (char string index) out)
                  (incf index)))
               ((char= ch #\")
                (incf index)
                (return (values (get-output-stream-string out) index)))
               (t
                (write-char ch out)
                (incf index)))
          finally (return (values (get-output-stream-string out) index)))))

(defun parse-atom-string (token)
  "Parse a non-delimiter token string into a keyword, number, or symbol."
  (cond
    ;; Keyword (:foo)
    ((and (>= (length token) 2) (char= (char token 0) #\:))
     (intern (string-upcase (subseq token 1)) :keyword))
    ;; Integer
    ((multiple-value-bind (val end) (parse-integer token :junk-allowed t)
       (when (and val (= end (length token)))
         val)))
    ;; Float or ratio
    ((let ((*read-eval* nil))
       (let ((parsed (ignore-errors (read-from-string token))))
         (when (numberp parsed)
           parsed))))
    ;; Standard symbol interned into *package*
    (t
     (intern (string-upcase token)))))

(defun tokenize (string)
  "Tokenize a string into a flat list of delimiter keywords and atomic values."
  (let ((index 0)
        (len (length string))
        (tokens '()))
    (loop while (< index len)
          for ch = (char string index)
          do (cond
               ((whitespace-p ch)
                (incf index))
               ((char= ch #\;)
                (incf index)
                (loop while (and (< index len) (not (char= (char string index) #\Newline)))
                      do (incf index))
                (when (and (< index len) (char= (char string index) #\Newline))
                  (incf index)))
               ((char= ch #\() (push :open-paren tokens) (incf index))
               ((char= ch #\)) (push :close-paren tokens) (incf index))
               ((char= ch #\[) (push :open-square tokens) (incf index))
               ((char= ch #\]) (push :close-square tokens) (incf index))
               ((char= ch #\{) (push :open-curly tokens) (incf index))
               ((char= ch #\}) (push :close-curly tokens) (incf index))
               ((char= ch #\")
                (multiple-value-bind (str next) (read-string-literal string index len)
                  (push str tokens)
                  (setf index next)))
               (t
                (let ((start index))
                  (loop while (and (< index len)
                                   (let ((c (char string index)))
                                     (not (or (whitespace-p c)
                                              (delimiter-p c)))))
                        do (incf index))
                  (push (parse-atom-string (subseq string start index)) tokens)))))
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
        ((list* (guard tok (eq tok close-token)) rest)
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
    ((list* :open-paren rest)
     (multiple-value-bind (children remaining) (parse-collection :close-paren rest path 0)
       (values (list* :path path :paren children) remaining)))
    ((list* :open-square rest)
     (multiple-value-bind (children remaining) (parse-collection :close-square rest path 0)
       (values (list* :path path :square children) remaining)))
    ((list* :open-curly rest)
     (multiple-value-bind (children remaining) (parse-collection :close-curly rest path 0)
       (values (list* :path path :curly children) remaining)))
    ((list* (or :close-paren :close-square :close-curly) _)
     (error 'sexp-parse-error
            :token (car tokens)
            :message (format nil "Unexpected closing delimiter: ~A" (car tokens))))
    ((list* atom rest)
     (values (list :path path :leaf atom) rest))))

(defun string-to-sexp (string)
  "Parse a raw string into an s-expression data structure.
Always returns a (:path () :file ...) node representing the parsed file contents."
  (let ((tokens (tokenize string)))
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

(defun format-collection (open close children indent)
  (if (null children)
      (format nil "~A~A" open close)
      (let ((child-strings (mapcar (lambda (c) (format-sexp c (+ indent 2)))
                                   children)))
        (let ((single-line (format nil "~A~{~A~^ ~}~A" open child-strings close)))
          (if (and (not (find #\Newline single-line))
                   (<= (length single-line) 60))
              single-line
              (with-output-to-string (out)
                (write-string open out)
                (write-string (first child-strings) out)
                (let ((indent-str (make-string (+ indent 2) :initial-element #\Space)))
                  (dolist (c (rest child-strings))
                    (terpri out)
                    (write-string indent-str out)
                    (write-string c out)))
                (write-string close out)))))))

(defun format-atom (val)
  (match val
    ((type string)
     (format nil "~S" val))
    ((type keyword)
     (format nil ":~A" (string-downcase (symbol-name val))))
    ((null)
     "()")
    ((type symbol)
     (string-downcase (symbol-name val)))
    ((type number)
     (format nil "~A" val))
    (_
     (format nil "~A" val))))

(defun format-sexp (expr indent)
  (match expr
    ;; Tagged leaf node: (:path _ :leaf val)
    ((leaf _ val)
     (format-atom val))
    ;; Tagged leaf node without :leaf: (:path _ val)
    ((list :path _ val)
     (format-atom val))
    ((node _ (or :file 'file) children)
     (format nil "~{~A~^~%~%~}" (mapcar (lambda (c) (format-sexp c indent)) children)))
    ((node _ (or :workspace 'workspace) children)
     (format nil "~{~A~^~%~%~}" (mapcar (lambda (c) (format-sexp c indent)) children)))
    ((node _ (or :paren 'paren) children)
     (format-collection "(" ")" children indent))
    ((node _ (or :square 'square) children)
     (format-collection "[" "]" children indent))
    ((node _ (or :curly 'curly) children)
     (format-collection "{" "}" children indent))
    ;; Backward-compatible untagged collections:
    ((list* (or :paren 'paren) children)
     (format-collection "(" ")" children indent))
    ((list* (or :square 'square) children)
     (format-collection "[" "]" children indent))
    ((list* (or :curly 'curly) children)
     (format-collection "{" "}" children indent))
    ((list* _ _)
     (format-collection "(" ")" expr indent))
    ;; Direct atoms:
    (_
     (format-atom expr))))

(defun sexp-to-string (expr &key (indent 0))
  "Serialize an s-expression back into its string representation with proper formatting."
  (format-sexp expr indent))
