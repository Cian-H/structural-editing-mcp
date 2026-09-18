(defpackage :structural-editing-mcp.analysis
  (:use :cl
        :alexandria
        :trivia
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser)
  (:export :variable-node-p
           :match-pattern
           :instantiate-pattern
           :search-ast
           :find-pattern-matches
           :*anti-pattern-rules*
           :lint-finding
           :make-lint-finding
           :lint-finding-rule
           :lint-finding-path
           :lint-finding-message
           :lint-finding-severity
           :lint-finding-suggested-fix
           :lint-node
           :lint-ast
           :format-lint-findings)
  (:documentation "Static analysis, pattern matching, structural search, and linting."))

(in-package :structural-editing-mcp.analysis)

(declaim (optimize (speed 2) (safety 3)))

;;; --- Pattern Matching Primitives ---

(defun variable-node-p (node)
  "Check if a leaf node is a pattern variable (symbol starting with ?)."
  (multiple-value-bind (path tag val) (parse-node node)
    (declare (ignore path))
    (and (eq tag :leaf)
         (symbolp val)
         (plusp (length (symbol-name val)))
         (char= (char (symbol-name val) 0) #\?))))

(defun match-pattern (pattern target bindings)
  "Match TARGET node against PATTERN node. Return (values success new-bindings)."
  (cond
    ((variable-node-p pattern)
     (let* ((var-name (nth-value 2 (parse-node pattern)))
            (existing (assoc var-name bindings)))
       (if existing
           (if (string= (sexp-to-string target) (sexp-to-string (cdr existing)))
               (values t bindings)
               (values nil bindings))
           (values t (cons (cons var-name target) bindings)))))
    ((and (eq (get-node-tag pattern) :leaf) (eq (get-node-tag target) :leaf))
     (let ((pval (nth-value 2 (parse-node pattern)))
           (tval (nth-value 2 (parse-node target))))
       (if (equal pval tval) (values t bindings) (values nil bindings))))
    ((and (member (get-node-tag pattern) '(:paren :square :curly))
          (eq (get-node-tag pattern) (get-node-tag target)))
     (let ((pchildren (get-node-children pattern))
           (tchildren (get-node-children target)))
       (if (= (length pchildren) (length tchildren))
           (loop for p in pchildren
                 for t-child in tchildren
                 do (multiple-value-bind (success new-bindings)
                        (match-pattern p t-child bindings)
                      (if success
                          (setf bindings new-bindings)
                          (return (values nil bindings))))
                 finally (return (values t bindings)))
           (values nil bindings))))
    (t (values nil bindings))))

(defun instantiate-pattern (pattern bindings)
  "Create a new AST node by substituting variables in PATTERN using BINDINGS."
  (cond
    ((variable-node-p pattern)
     (let* ((var-name (nth-value 2 (parse-node pattern)))
            (bound (cdr (assoc var-name bindings))))
       (if bound bound pattern)))
    ((member (get-node-tag pattern) '(:leaf :comment)) pattern)
    (t
     (let ((children (get-node-children pattern)))
       (list* :path (get-node-path pattern)
              (get-node-tag pattern)
              (mapcar (lambda (c) (instantiate-pattern c bindings)) children))))))

;;; --- Search Primitives ---

(defun search-ast (tree query &key path exact)
  "Search the AST in TREE (optionally starting under PATH) for leaf nodes matching QUERY.
If EXACT is T, requires exact match; otherwise searches case-insensitively for substrings."
  (let ((results '())
        (lower-query (string-downcase query))
        (start-node (if (and path (not (null path)))
                        (get-node-at-path tree path)
                        tree)))
    (when start-node
      (labels ((walk (node)
                 (match node
                   ((leaf node-path val)
                    (let* ((str (format-atom val))
                           (lower-str (string-downcase str)))
                      (when (if exact
                                (string= lower-query lower-str)
                                (search lower-query lower-str))
                        (push node-path results))))
                   ((node _ _ children)
                    (dolist (child children)
                      (walk child)))
                   (_ nil))))
        (walk start-node)))
    (nreverse results)))

(defun find-pattern-matches (tree pattern-str-or-ast &key path)
  "Find all subtrees in TREE (or under PATH) that match PATTERN-STR-OR-AST.
Returns a list of plists: (:path <path> :node <node> :bindings <bindings>)."
  (let* ((pattern-ast (if (stringp pattern-str-or-ast)
                          (first (get-node-children (string-to-sexp pattern-str-or-ast)))
                          pattern-str-or-ast))
         (start-node (if (and path (not (null path)))
                         (get-node-at-path tree path)
                         tree))
         (matches '()))
    (when (and pattern-ast start-node)
      (labels ((walk (node)
                 (multiple-value-bind (matched-p bindings)
                     (match-pattern pattern-ast node nil)
                   (when matched-p
                     (push (list :path (get-node-path node)
                                 :node node
                                 :bindings bindings)
                           matches))
                   (let ((children (get-node-children node)))
                     (dolist (child children)
                       (walk child))))))
        (walk start-node)))
    (nreverse matches)))

;;; --- Lint Finding Structure ---

(defstruct (lint-finding (:constructor make-lint-finding))
  rule
  path
  message
  severity      ; :style, :warning, :info
  suggested-fix ; nil, or plist (:pattern ... :replacement ...), or string
  )

;;; --- Node Inspection Helpers for Linting Rules ---

(defun leaf-symbol-p (node name)
  "Return T if NODE is a leaf symbol matching NAME (case-insensitive string or symbol)."
  (multiple-value-bind (path tag val) (parse-node node)
    (declare (ignore path))
    (and (eq tag :leaf)
         (symbolp val)
         (string-equal (symbol-name val) (string name)))))

(defun leaf-nil-p (node)
  "Return T if NODE is the symbol NIL."
  (multiple-value-bind (path tag val) (parse-node node)
    (declare (ignore path))
    (and (eq tag :leaf)
         (or (null val)
             (and (symbolp val) (string-equal (symbol-name val) "NIL"))))))

(defun leaf-true-p (node &optional (dialect :common-lisp))
  "Return T if NODE represents boolean true."
  (multiple-value-bind (path tag val) (parse-node node)
    (declare (ignore path))
    (and (eq tag :leaf)
         (or (eq val t)
             (and (symbolp val)
                  (or (string-equal (symbol-name val) "T")
                      (and (eq dialect :clojure) (string-equal (symbol-name val) "TRUE"))))))))

(defun leaf-false-p (node &optional (dialect :common-lisp))
  "Return T if NODE represents boolean false or nil."
  (or (leaf-nil-p node)
      (multiple-value-bind (path tag val) (parse-node node)
        (declare (ignore path))
        (and (eq tag :leaf)
             (symbolp val)
             (and (eq dialect :clojure) (string-equal (symbol-name val) "FALSE"))))))

;;; --- Rule Implementations ---

(defun check-if-progn-to-when (node path dialect)
  "Detect (if <cond> (progn <body...>)) or (if <cond> (progn <body...>) nil)."
  (declare (ignore dialect))
  (let ((children (get-node-children node)))
    (when (and (member (get-node-tag node) '(:paren :square))
               (or (= (length children) 3)
                   (and (= (length children) 4) (leaf-nil-p (fourth children))))
               (leaf-symbol-p (first children) "IF"))
      (let* ((cond-node (second children))
             (then-node (third children))
             (then-children (get-node-children then-node)))
        (when (and (member (get-node-tag then-node) '(:paren :square))
                   then-children
                   (leaf-symbol-p (first then-children) "PROGN"))
          (let* ((body-nodes (rest then-children))
                 (body-str (if body-nodes
                               (format nil "~{~A~^ ~}" (mapcar #'sexp-to-string body-nodes))
                               "nil"))
                 (replacement (format nil "(when ~A ~A)" (sexp-to-string cond-node) body-str)))
            (make-lint-finding
             :rule :if-progn-to-when
             :path path
             :message "Prefer '(when ...)' over '(if ... (progn ...))' when there is no else branch."
             :severity :style
             :suggested-fix replacement)))))))

(defun check-if-nil-to-when (node path dialect)
  "Detect (if <cond> <then> nil) where <then> is not progn, not boolean true, and <cond> is not (not ...)."
  (let ((children (get-node-children node)))
    (when (and (member (get-node-tag node) '(:paren :square))
               (= (length children) 4)
               (leaf-symbol-p (first children) "IF")
               (leaf-nil-p (fourth children)))
      (let* ((cond-node (second children))
             (then-node (third children))
             (cond-children (get-node-children cond-node))
             (then-children (get-node-children then-node)))
        ;; Don't fire if then is boolean true (handled by check-if-boolean-redundant)
        ;; Don't fire if cond is (not ...) (handled by check-if-not-to-unless)
        ;; Don't fire if then is progn (handled by check-if-progn-to-when)
        (unless (or (leaf-true-p then-node dialect)
                    (and (member (get-node-tag cond-node) '(:paren :square))
                         (= (length cond-children) 2)
                         (leaf-symbol-p (first cond-children) "NOT"))
                    (and (member (get-node-tag then-node) '(:paren :square))
                         then-children
                         (leaf-symbol-p (first then-children) "PROGN")))
          (let ((replacement (format nil "(when ~A ~A)"
                                     (sexp-to-string cond-node)
                                     (sexp-to-string then-node))))
            (make-lint-finding
             :rule :if-nil-to-when
             :path path
             :message "Prefer '(when <cond> <then>)' over '(if <cond> <then> nil)'."
             :severity :style
             :suggested-fix replacement)))))))

(defun check-if-not-to-unless (node path dialect)
  "Detect (if (not <cond>) <then> nil) or (if (not <cond>) <then>)."
  (declare (ignore dialect))
  (let ((children (get-node-children node)))
    (when (and (member (get-node-tag node) '(:paren :square))
               (or (= (length children) 3)
                   (and (= (length children) 4) (leaf-nil-p (fourth children))))
               (leaf-symbol-p (first children) "IF"))
      (let* ((test-node (second children))
             (test-children (get-node-children test-node)))
        (when (and (member (get-node-tag test-node) '(:paren :square))
                   (= (length test-children) 2)
                   (leaf-symbol-p (first test-children) "NOT"))
          (let* ((inner-cond (second test-children))
                 (then-node (third children))
                 (replacement (format nil "(unless ~A ~A)"
                                      (sexp-to-string inner-cond)
                                      (sexp-to-string then-node))))
            (make-lint-finding
             :rule :if-not-to-unless
             :path path
             :message "Prefer '(unless <cond> <then>)' over '(if (not <cond>) <then>)'."
             :severity :style
             :suggested-fix replacement)))))))

(defun check-invert-if-not (node path dialect)
  "Detect (if (not <cond>) <then> <else>) where <else> is not nil."
  (declare (ignore dialect))
  (let ((children (get-node-children node)))
    (when (and (member (get-node-tag node) '(:paren :square))
               (= (length children) 4)
               (leaf-symbol-p (first children) "IF")
               (not (leaf-nil-p (fourth children))))
      (let* ((test-node (second children))
             (test-children (get-node-children test-node)))
        (when (and (member (get-node-tag test-node) '(:paren :square))
                   (= (length test-children) 2)
                   (leaf-symbol-p (first test-children) "NOT"))
          (let* ((inner-cond (second test-children))
                 (then-node (third children))
                 (else-node (fourth children))
                 (replacement (format nil "(if ~A ~A ~A)"
                                      (sexp-to-string inner-cond)
                                      (sexp-to-string else-node)
                                      (sexp-to-string then-node))))
            (make-lint-finding
             :rule :invert-if-not
             :path path
             :message "Invert negated condition: replace '(if (not <cond>) <then> <else>)' with '(if <cond> <else> <then>)'."
             :severity :style
             :suggested-fix replacement)))))))

(defun check-single-clause-cond (node path dialect)
  "Detect (cond (<test> <body...>)) with a single clause."
  (declare (ignore dialect))
  (let ((children (get-node-children node)))
    (when (and (member (get-node-tag node) '(:paren :square))
               (= (length children) 2)
               (leaf-symbol-p (first children) "COND"))
      (let* ((clause (second children))
             (clause-children (get-node-children clause)))
        (when (and (member (get-node-tag clause) '(:paren :square))
                   (>= (length clause-children) 2)
                   (not (leaf-true-p (first clause-children) dialect))
                   (not (leaf-symbol-p (first clause-children) "OTHERWISE"))
                   (not (leaf-symbol-p (first clause-children) ":ELSE")))
          (let* ((test-node (first clause-children))
                 (body-nodes (rest clause-children))
                 (replacement (format nil "(when ~A ~{~A~^ ~})"
                                      (sexp-to-string test-node)
                                      (mapcar #'sexp-to-string body-nodes))))
            (make-lint-finding
             :rule :single-clause-cond
             :path path
             :message "'cond' with a single clause can be simplified to '(when <test> <body...>)'."
             :severity :style
             :suggested-fix replacement)))))))

(defun check-if-boolean-redundant (node path dialect)
  "Detect (if <cond> t nil) or (if <cond> true false)."
  (let ((children (get-node-children node)))
    (when (and (member (get-node-tag node) '(:paren :square))
               (= (length children) 4)
               (leaf-symbol-p (first children) "IF")
               (leaf-true-p (third children) dialect)
               (leaf-false-p (fourth children) dialect))
      (let* ((cond-node (second children))
             (replacement (if (eq dialect :clojure)
                              (format nil "(boolean ~A)" (sexp-to-string cond-node))
                              (format nil "(not (null ~A))" (sexp-to-string cond-node)))))
        (make-lint-finding
         :rule :if-boolean-redundant
         :path path
         :message "Redundant conditional returning boolean; simplify '(if <cond> t nil)'."
         :severity :warning
         :suggested-fix replacement)))))

(defun check-redundant-progn (node path dialect)
  "Detect (progn <single-expression>)."
  (declare (ignore dialect))
  (let ((children (get-node-children node)))
    (when (and (member (get-node-tag node) '(:paren :square))
               (= (length children) 2)
               (leaf-symbol-p (first children) "PROGN"))
      (let ((single-node (second children)))
        (make-lint-finding
         :rule :redundant-progn
         :path path
         :message "Single-expression 'progn' is redundant."
         :severity :style
         :suggested-fix (sexp-to-string single-node))))))

(defun check-nested-let (node path dialect)
  "Detect nested (let ((x ...)) (let ((y ...)) ...)) that could be combined into let*."
  (declare (ignore dialect))
  (let ((children (get-node-children node)))
    (when (and (member (get-node-tag node) '(:paren :square))
               (>= (length children) 3)
               (leaf-symbol-p (first children) "LET"))
      (let* ((outer-bindings (second children))
             (outer-body (nthcdr 2 children)))
        (when (and (= (length outer-body) 1)
                   (member (get-node-tag outer-bindings) '(:paren :square)))
          (let* ((inner-node (first outer-body))
                 (inner-children (get-node-children inner-node)))
            (when (and (member (get-node-tag inner-node) '(:paren :square))
                       (>= (length inner-children) 3)
                       (leaf-symbol-p (first inner-children) "LET"))
              (let* ((inner-bindings (second inner-children))
                     (inner-body (nthcdr 2 inner-children)))
                (when (member (get-node-tag inner-bindings) '(:paren :square))
                  (let* ((all-bindings (append (get-node-children outer-bindings)
                                               (get-node-children inner-bindings)))
                         (bindings-str (format nil "(~{~A~^ ~})" (mapcar #'sexp-to-string all-bindings)))
                         (body-str (format nil "~{~A~^ ~}" (mapcar #'sexp-to-string inner-body)))
                         (replacement (format nil "(let* ~A ~A)" bindings-str body-str)))
                    (make-lint-finding
                     :rule :nested-let
                     :path path
                     :message "Cascaded nested 'let' forms can be combined into a single 'let*'."
                     :severity :style
                     :suggested-fix replacement)))))))))))

(defun check-equal-nil-to-null (node path dialect)
  "Detect (equal ?x nil), (eq ?x nil), or (eql ?x nil) in Common Lisp / Elisp."
  (when (member dialect '(:common-lisp :emacs-lisp nil))
    (let ((children (get-node-children node)))
      (when (and (member (get-node-tag node) '(:paren :square))
                 (= (length children) 3)
                 (or (leaf-symbol-p (first children) "EQUAL")
                     (leaf-symbol-p (first children) "EQ")
                     (leaf-symbol-p (first children) "EQL")))
        (cond
          ((leaf-nil-p (third children))
           (let ((replacement (format nil "(null ~A)" (sexp-to-string (second children)))))
             (make-lint-finding
              :rule :equal-nil-to-null
              :path path
              :message (format nil "Prefer '(null ~A)' over comparison with nil."
                               (sexp-to-string (second children)))
              :severity :style
              :suggested-fix replacement)))
          ((leaf-nil-p (second children))
           (let ((replacement (format nil "(null ~A)" (sexp-to-string (third children)))))
             (make-lint-finding
              :rule :equal-nil-to-null
              :path path
              :message (format nil "Prefer '(null ~A)' over comparison with nil."
                               (sexp-to-string (third children)))
              :severity :style
              :suggested-fix replacement))))))))

;;; --- Catalog of Anti-Pattern Rules ---

(defparameter *anti-pattern-rules*
  (list
   (list :id :if-progn-to-when
         :check #'check-if-progn-to-when
         :dialects '(:common-lisp :emacs-lisp :scheme))
   (list :id :if-nil-to-when
         :check #'check-if-nil-to-when
         :dialects '(:common-lisp :emacs-lisp :scheme :clojure))
   (list :id :if-not-to-unless
         :check #'check-if-not-to-unless
         :dialects '(:common-lisp :emacs-lisp :clojure))
   (list :id :invert-if-not
         :check #'check-invert-if-not
         :dialects nil)
   (list :id :single-clause-cond
         :check #'check-single-clause-cond
         :dialects '(:common-lisp :emacs-lisp :scheme :clojure))
   (list :id :if-boolean-redundant
         :check #'check-if-boolean-redundant
         :dialects nil)
   (list :id :redundant-progn
         :check #'check-redundant-progn
         :dialects '(:common-lisp :emacs-lisp))
   (list :id :nested-let
         :check #'check-nested-let
         :dialects '(:common-lisp :emacs-lisp :scheme))
   (list :id :equal-nil-to-null
         :check #'check-equal-nil-to-null
         :dialects '(:common-lisp :emacs-lisp)))
  "Active structural anti-pattern and code smell lint rules.")

;;; --- Lint Engine ---

(defun rule-matches-dialect-p (rule dialect)
  "Return T if RULE applies to DIALECT."
  (let ((rule-dialects (getf rule :dialects)))
    (or (null rule-dialects)
        (null dialect)
        (member dialect rule-dialects))))

(defun rule-matches-filter-p (rule-id requested-rules)
  "Return T if RULE-ID is permitted by REQUESTED-RULES (list of keywords or strings)."
  (if (null requested-rules)
      t
      (member (string-downcase (string rule-id))
              (mapcar (lambda (r) (string-downcase (string r))) requested-rules)
              :test #'string=)))

(defun lint-node (node path &key (dialect :common-lisp) rules)
  "Check a single NODE against active rules. Return a list of LINT-FINDING instances."
  (let ((findings '()))
    (dolist (r *anti-pattern-rules*)
      (let ((id (getf r :id))
            (check-fn (getf r :check)))
        (when (and (rule-matches-dialect-p r dialect)
                   (rule-matches-filter-p id rules)
                   check-fn)
          (let ((finding (funcall check-fn node path dialect)))
            (when finding
              (push finding findings))))))
    (nreverse findings)))

(defun lint-ast (tree &key path dialect rules)
  "Recursively lint TREE (or subtree at PATH) for structural code smells and anti-patterns.
Returns a list of LINT-FINDING instances."
  (let* ((start-node (if (and path (not (null path)))
                         (get-node-at-path tree path)
                         tree))
         (findings '()))
    (when start-node
      (labels ((walk (node current-dialect)
                 (let* ((tag (get-node-tag node))
                        (node-path (get-node-path node))
                        ;; Track dialect transitions if traversing workspace/dialect nodes
                        (effective-dialect
                          (cond
                            ((member tag '(:common-lisp :clojure :scheme :emacs-lisp :fennel))
                             tag)
                            (t (or current-dialect dialect :common-lisp))))
                        (node-findings (lint-node node node-path
                                                  :dialect effective-dialect
                                                  :rules rules)))
                   (dolist (f node-findings)
                     (push f findings))
                   (let ((children (get-node-children node)))
                     (dolist (child children)
                       (walk child effective-dialect))))))
        (walk start-node dialect)))
    (nreverse findings)))

(defun format-lint-findings (findings)
  "Format a list of LINT-FINDING instances into a human-readable diagnostic report."
  (if (null findings)
      "No anti-patterns or code smells detected."
      (with-output-to-string (s)
        (format s "Found ~A anti-pattern~:P:~%~%" (length findings))
        (loop for f in findings
              for i from 1
              for rule = (lint-finding-rule f)
              for path = (lint-finding-path f)
              for msg = (lint-finding-message f)
              for sev = (lint-finding-severity f)
              for fix = (lint-finding-suggested-fix f)
              do
              (format s "~A. [~A] [~{~A~^, ~}] ~A~%   Message: ~A~%"
                      i sev (or path "()") (string-downcase (string rule)) msg)
              (when fix
                (cond
                  ((and (listp fix) (getf fix :pattern) (getf fix :replacement))
                   (format s "   Suggested Fix:~%     Pattern:     ~A~%     Replacement: ~A~%"
                           (getf fix :pattern) (getf fix :replacement)))
                  ((stringp fix)
                   (format s "   Suggested Fix: ~A~%" fix))))
              (format s "~%")))))
