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
           :format-lint-findings
           :complexity-metrics
           :make-complexity-metrics
           :complexity-metrics-name
           :complexity-metrics-kind
           :complexity-metrics-path
           :complexity-metrics-cyclomatic-complexity
           :complexity-metrics-max-nesting-depth
           :complexity-metrics-form-count
           :complexity-metrics-recommendations
           :form-definition-info
           :compute-branch-complexity
           :compute-nesting-depth
           :count-ast-nodes
           :analyze-form-complexity
           :analyze-complexity
           :format-complexity-report)
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

;;; --- Complexity Metrics ---

(defstruct (complexity-metrics (:constructor make-complexity-metrics))
  name
  kind ; :function, :macro, :method, :generic, :form
  path
  cyclomatic-complexity
  max-nesting-depth
  form-count
  recommendations)

(defun form-definition-info (node)
  "If NODE is a definition (defun, defmacro, defmethod, defgeneric, defn, define),
return (values is-def-p name-str kind-keyword)."
  (let ((children (get-node-children node)))
    (when (and (member (get-node-tag node) '(:paren :square))
               (>= (length children) 2))
      (let ((head (first children)))
        (multiple-value-bind (path tag val) (parse-node head)
          (declare (ignore path tag))
          (when (symbolp val)
            (let ((head-name (string-upcase (symbol-name val))))
              (cond
                ((member head-name '("DEFUN" "DEFN" "DEFN-") :test #'string=)
                 (let ((name-node (second children)))
                   (values t (format-atom (nth-value 2 (parse-node name-node))) :function)))
                ((member head-name '("DEFMACRO" "DEFSYNTAX") :test #'string=)
                 (let ((name-node (second children)))
                   (values t (format-atom (nth-value 2 (parse-node name-node))) :macro)))
                ((member head-name '("DEFMETHOD") :test #'string=)
                 (let ((name-node (second children)))
                   (values t (format-atom (nth-value 2 (parse-node name-node))) :method)))
                ((member head-name '("DEFGENERIC") :test #'string=)
                 (let ((name-node (second children)))
                   (values t (format-atom (nth-value 2 (parse-node name-node))) :generic)))
                ((string= head-name "DEFINE")
                 (let ((name-child (second children)))
                   (if (member (get-node-tag name-child) '(:paren :square))
                       (let ((fn-head (first (get-node-children name-child))))
                         (values t (format-atom (nth-value 2 (parse-node fn-head))) :function))
                       (values t (format-atom (nth-value 2 (parse-node name-child))) :definition))))
                (t (values nil nil nil))))))))))

(defun compute-branch-complexity (node &optional (dialect :common-lisp))
  "Compute McCabe cyclomatic complexity of NODE.
Base complexity is 1, with +1 for each conditional branch, short-circuit point, loop, or handler."
  (let ((complexity 1))
    (labels ((walk (curr)
               (let ((children (get-node-children curr)))
                 (when (and (member (get-node-tag curr) '(:paren :square))
                            children)
                   (let ((head (first children)))
                     (multiple-value-bind (p t-val val) (parse-node head)
                       (declare (ignore p t-val))
                       (when (symbolp val)
                         (let ((name (string-upcase (symbol-name val))))
                           (cond
                             ;; Standard conditional constructs: +1
                             ((member name '("IF" "WHEN" "UNLESS" "WHEN-NOT" "IF-NOT"
                                             "WHEN-LET" "IF-LET" "WHEN-FIRST")
                                      :test #'string=)
                              (incf complexity))
                             ;; COND: each non-default test clause adds +1
                             ((string= name "COND")
                              (dolist (clause (rest children))
                                (let ((c-children (get-node-children clause)))
                                  (when (and (member (get-node-tag clause) '(:paren :square))
                                             c-children)
                                    (let ((test (first c-children)))
                                      (unless (or (leaf-true-p test dialect)
                                                  (leaf-symbol-p test "OTHERWISE")
                                                  (leaf-symbol-p test ":ELSE"))
                                        (incf complexity)))))))
                             ;; CASE/TYPECASE constructs: each clause adds +1
                             ((member name '("CASE" "CCASE" "ECASE" "TYPECASE" "CTYPECASE"
                                             "ETYPECASE" "CONDP")
                                      :test #'string=)
                              (dolist (clause (nthcdr 2 children))
                                (let ((c-children (get-node-children clause)))
                                  (when (and (member (get-node-tag clause) '(:paren :square))
                                             c-children)
                                    (let ((selector (first c-children)))
                                      (unless (or (leaf-true-p selector dialect)
                                                  (leaf-symbol-p selector "OTHERWISE"))
                                        (incf complexity)))))))
                             ;; Short-circuiting booleans: each extra operand adds +1
                             ((member name '("AND" "OR") :test #'string=)
                              (when (> (length children) 2)
                                (incf complexity (- (length children) 2))))
                             ;; Loops: +1
                             ((member name '("LOOP" "DOLIST" "DOTIMES" "DO" "DO*" "DOSEQ" "RECUR")
                                      :test #'string=)
                              (incf complexity))
                             ;; Error & condition handlers: each clause adds +1
                             ((member name '("HANDLER-CASE" "RESTART-CASE") :test #'string=)
                              (dolist (clause (nthcdr 2 children))
                                (when (member (get-node-tag clause) '(:paren :square))
                                  (incf complexity))))))))))
                 ;; Recurse into children
                 (dolist (c children)
                   (walk c)))))
      (walk node)
      complexity)))

(defun compute-nesting-depth (node &optional (current-depth 0))
  "Compute the maximum parenthetical nesting depth of sub-expressions within NODE."
  (let ((children (get-node-children node)))
    (if (and (member (get-node-tag node) '(:paren :square :curly))
             children)
        (let ((next-depth (1+ current-depth))
              (max-child-depth (1+ current-depth)))
          (dolist (c children)
            (let ((d (compute-nesting-depth c next-depth)))
              (when (> d max-child-depth)
                (setf max-child-depth d))))
          max-child-depth)
        current-depth)))

(defun count-ast-nodes (node)
  "Count total number of nodes (forms and leaves) in NODE."
  (let ((count 1))
    (dolist (c (get-node-children node))
      (incf count (count-ast-nodes c)))
    count))

(defun generate-complexity-recommendations (complexity depth count path)
  "Produce actionable refactoring recommendations when metrics exceed thresholds."
  (declare (ignore path))
  (let ((recs '()))
    (when (>= complexity 10)
      (push (format nil "High cyclomatic complexity (~A) — consider decomposing conditional logic into helper functions using 'ast_extract_function'."
                    complexity)
            recs))
    (when (>= depth 6)
      (push (format nil "Deep nesting depth (~A) — consider flattening expressions or extracting intermediate values using 'ast_extract_variable'."
                    depth)
            recs))
    (when (>= count 60)
      (push (format nil "Large form size (~A nodes) — consider breaking this definition into smaller, single-purpose functions."
                    count)
            recs))
    (nreverse recs)))

(defun analyze-form-complexity (node &key path dialect)
  "Analyze structural complexity of a single top-level form or function definition NODE."
  (multiple-value-bind (is-def name kind) (form-definition-info node)
    (let* ((effective-name (if is-def name (let ((tag (get-node-tag node))) (format nil ":~A" tag))))
           (effective-kind (if is-def kind :form))
           (complexity (compute-branch-complexity node dialect))
           (depth (compute-nesting-depth node 0))
           (node-count (count-ast-nodes node))
           (recs (generate-complexity-recommendations complexity depth node-count path)))
      (make-complexity-metrics
       :name effective-name
       :kind effective-kind
       :path path
       :cyclomatic-complexity complexity
       :max-nesting-depth depth
       :form-count node-count
       :recommendations recs))))

(defun collect-top-level-forms (node base-path)
  "Collect all (form-node . path) pairs from NODE (workspace, dialect, file, or single form)."
  (let ((tag (get-node-tag node))
        (results '()))
    (cond
      ((eq tag :workspace)
       (loop for dialect-child in (get-node-children node)
             for d-idx from 0
             for d-path = (or (get-node-path dialect-child) (append base-path (list d-idx)))
             do (loop for file-child in (get-node-children dialect-child)
                      for f-idx from 0
                      for f-path = (or (get-node-path file-child) (append d-path (list f-idx)))
                      do (loop for form in (get-node-children file-child)
                               for form-idx from 0
                               for form-path = (or (get-node-path form) (append f-path (list form-idx)))
                               do (push (cons form form-path) results)))))
      ((member tag '(:common-lisp :clojure :scheme :emacs-lisp :fennel))
       (loop for file-child in (get-node-children node)
             for f-idx from 0
             for f-path = (or (get-node-path file-child) (append base-path (list f-idx)))
             do (loop for form in (get-node-children file-child)
                      for form-idx from 0
                      for form-path = (or (get-node-path form) (append f-path (list form-idx)))
                      do (push (cons form form-path) results))))
      ((eq tag :file)
       (loop for form in (get-node-children node)
             for form-idx from 0
             for form-path = (or (get-node-path form) (append base-path (list form-idx)))
             do (push (cons form form-path) results)))
      (t
       (push (cons node (or (get-node-path node) base-path)) results)))
    (nreverse results)))

(defun analyze-complexity (tree &key path dialect (min-complexity 1) (min-depth 1))
  "Analyze structural complexity for forms in TREE (or under PATH).
Filters results to those meeting MIN-COMPLEXITY and MIN-DEPTH thresholds."
  (let* ((start-node (if (and path (not (null path)))
                         (get-node-at-path tree path)
                         tree))
         (forms-with-paths (if start-node
                               (collect-top-level-forms start-node path)
                               nil))
         (results '()))
    (dolist (pair forms-with-paths)
      (let* ((form-node (car pair))
             (form-path (cdr pair))
             (metrics (analyze-form-complexity form-node :path form-path :dialect dialect)))
        (when (and (>= (complexity-metrics-cyclomatic-complexity metrics) (or min-complexity 1))
                   (>= (complexity-metrics-max-nesting-depth metrics) (or min-depth 1)))
          (push metrics results))))
    ;; Sort by cyclomatic complexity descending, then max-nesting-depth descending
    (sort results
          (lambda (a b)
            (let ((ca (complexity-metrics-cyclomatic-complexity a))
                  (cb (complexity-metrics-cyclomatic-complexity b)))
              (if (= ca cb)
                  (> (complexity-metrics-max-nesting-depth a)
                     (complexity-metrics-max-nesting-depth b))
                  (> ca cb)))))))

(defun format-complexity-report (results)
  "Format a list of COMPLEXITY-METRICS instances into a readable diagnostic report."
  (if (null results)
      "No forms found matching the specified complexity thresholds."
      (with-output-to-string (s)
        (format s "Structural Complexity Report (~A form~:P analyzed):~%~%" (length results))
        (loop for m in results
              for i from 1
              for name = (complexity-metrics-name m)
              for kind = (complexity-metrics-kind m)
              for path = (complexity-metrics-path m)
              for cc = (complexity-metrics-cyclomatic-complexity m)
              for depth = (complexity-metrics-max-nesting-depth m)
              for count = (complexity-metrics-form-count m)
              for recs = (complexity-metrics-recommendations m)
              do
              (format s "~A. ~A ~A [~{~A~^, ~}]~%   Cyclomatic Complexity: ~A | Max Nesting Depth: ~A | AST Nodes: ~A~%"
                      i kind name (or path "()") cc depth count)
              (when recs
                (format s "   Recommendations:~%")
                (dolist (r recs)
                  (format s "     - ~A~%" r)))
              (format s "~%")))))

