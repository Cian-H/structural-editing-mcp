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
           :if-form-p
           :not-form-cond
           :progn-form-body-nodes
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
           :format-complexity-report
           :duplicate-group
           :make-duplicate-group
           :duplicate-group-code-snippet
           :duplicate-group-occurrence-count
           :duplicate-group-paths
           :duplicate-group-node-count
           :duplicate-group-depth
           :duplicate-group-recommendation
           :canonicalize-subtree
           :find-duplicate-subtrees
           :format-duplicate-report
           :binding-finding
           :make-binding-finding
           :binding-finding-kind
           :binding-finding-variable-name
           :binding-finding-path
           :binding-finding-scope-kind
           :binding-finding-outer-path
           :binding-finding-message
           :binding-finding-recommendation
           :vector-binding-dialect-p
           :lisp-1-dialect-p
           :analyze-bindings
           :format-unused-bindings
           :format-shadowed-bindings
           :format-binding-report
           :refactoring-suggestion
           :make-refactoring-suggestion
           :refactoring-suggestion-category
           :refactoring-suggestion-priority
           :refactoring-suggestion-path
           :refactoring-suggestion-description
           :refactoring-suggestion-recommended-tool
           :refactoring-suggestion-action-plan
           :suggest-refactorings
           :format-refactoring-suggestions)
  (:documentation "Static analysis, pattern matching, structural search, and linting."))

(in-package :structural-editing-mcp.analysis)

(declaim (optimize (speed 2) (safety 3)))

(defun variable-node-p (node)
  "Check if a leaf node is a pattern variable (symbol starting with ?)."
  (multiple-value-bind (path tag val) (parse-node node)
    (declare (ignore path))
    (and (eq tag :leaf)
         (symbolp val)
         (plusp (length (symbol-name val)))
         (char= (char (symbol-name val) 0) #\?))))

(defun match-variable-pattern (pattern target bindings)
  "Match TARGET against pattern variable PATTERN."
  (let* ((var-name (nth-value 2 (parse-node pattern)))
         (existing (assoc var-name bindings)))
    (if existing
        (if (string= (sexp-to-string target) (sexp-to-string (cdr existing)))
            (values t bindings)
            (values nil bindings))
        (values t (cons (cons var-name target) bindings)))))

(defun match-leaf-pattern (pattern target bindings)
  "Match leaf TARGET against leaf PATTERN."
  (let ((pval (nth-value 2 (parse-node pattern)))
        (tval (nth-value 2 (parse-node target))))
    (if (equal pval tval)
        (values t bindings)
        (values nil bindings))))

(defun match-children-patterns (pchildren tchildren bindings)
  "Match sequences of pattern children and target children."
  (if (= (length pchildren) (length tchildren))
      (loop for p in pchildren
            for t-child in tchildren
            do (multiple-value-bind (success new-bindings)
                   (match-pattern p t-child bindings)
                 (if success
                     (setf bindings new-bindings)
                     (return (values nil bindings))))
            finally (return (values t bindings)))
      (values nil bindings)))

(defun match-pattern (pattern target bindings)
  "Match TARGET node against PATTERN node. Return (values success new-bindings)."
  (cond
    ((variable-node-p pattern)
     (match-variable-pattern pattern target bindings))
    ((and (eq (get-node-tag pattern) :leaf) (eq (get-node-tag target) :leaf))
     (match-leaf-pattern pattern target bindings))
    ((and (member (get-node-tag pattern) '(:paren :square :curly))
          (eq (get-node-tag pattern) (get-node-tag target)))
     (match-children-patterns (get-node-children pattern) (get-node-children target) bindings))
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

(defun search-ast (tree query &key path exact)
  "Search the AST in TREE (optionally starting under PATH) for leaf nodes matching QUERY.
If EXACT is T, requires exact match; otherwise searches case-insensitively for substrings."
  (let ((results '())
        (lower-query (string-downcase query))
        (start-node (resolve-tree-scope tree path)))
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
         (start-node (resolve-tree-scope tree path))
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

(defstruct (lint-finding (:constructor make-lint-finding))
  rule
  path
  message
  severity
  suggested-fix)

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

(defun if-form-p (node)
  "Return T if NODE is a compound (if ...) form with 3 or 4 elements."
  (and (compound-node-p node)
       (let ((children (get-node-children node)))
         (and (or (= (length children) 3)
                  (= (length children) 4))
              (leaf-symbol-p (first children) "IF")))))

(defun not-form-cond (node)
  "If NODE is a compound (not <cond>) form, return the inner condition node; otherwise NIL."
  (when (and (compound-node-p node)
             (let ((children (get-node-children node)))
               (and (= (length children) 2)
                    (leaf-symbol-p (first children) "NOT"))))
    (second (get-node-children node))))

(defun single-branch-if-p (node)
  "Return T if NODE is a compound (if ...) form with no else branch or an explicit nil else branch."
  (and (if-form-p node)
       (let ((children (get-node-children node)))
         (or (= (length children) 3)
             (leaf-nil-p (fourth children))))))

(defun two-branch-if-p (node)
  "Return T if NODE is a compound (if ...) form with an explicit non-nil else branch."
  (and (if-form-p node)
       (let ((children (get-node-children node)))
         (and (= (length children) 4)
              (not (leaf-nil-p (fourth children)))))))

(defun progn-form-body-nodes (node)
  "If NODE is a compound (progn <body...>) form, return (values T body-nodes); otherwise (values NIL NIL)."
  (if (and (compound-node-p node)
           (let ((children (get-node-children node)))
             (and children (leaf-symbol-p (first children) "PROGN"))))
      (values t (rest (get-node-children node)))
      (values nil nil)))

(defun check-if-progn-to-when (node path dialect)
  "Detect (if <cond> (progn <body...>)) or (if <cond> (progn <body...>) nil)."
  (declare (ignore dialect))
  (when (single-branch-if-p node)
    (let* ((children (get-node-children node))
           (cond-node (second children))
           (then-node (third children)))
      (multiple-value-bind (is-progn body-nodes) (progn-form-body-nodes then-node)
        (when is-progn
          (let* ((body-str (if body-nodes
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
    (when (and (if-form-p node)
               (= (length children) 4)
               (leaf-nil-p (fourth children)))
      (let ((cond-node (second children))
            (then-node (third children)))
        (unless (or (leaf-true-p then-node dialect)
                    (not-form-cond cond-node)
                    (nth-value 0 (progn-form-body-nodes then-node)))
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
  (when (single-branch-if-p node)
    (let* ((children (get-node-children node))
           (inner-cond (not-form-cond (second children)))
           (then-node (third children)))
      (when inner-cond
        (let ((replacement (format nil "(unless ~A ~A)"
                                   (sexp-to-string inner-cond)
                                   (sexp-to-string then-node))))
          (make-lint-finding
           :rule :if-not-to-unless
           :path path
           :message "Prefer '(unless <cond> <then>)' over '(if (not <cond>) <then>)'."
           :severity :style
           :suggested-fix replacement))))))

(defun check-invert-if-not (node path dialect)
  "Detect (if (not <cond>) <then> <else>) where <else> is not nil."
  (declare (ignore dialect))
  (when (two-branch-if-p node)
    (let* ((children (get-node-children node))
           (inner-cond (not-form-cond (second children)))
           (then-node (third children))
           (else-node (fourth children)))
      (when inner-cond
        (let ((replacement (format nil "(if ~A ~A ~A)"
                                   (sexp-to-string inner-cond)
                                   (sexp-to-string else-node)
                                   (sexp-to-string then-node))))
          (make-lint-finding
           :rule :invert-if-not
           :path path
           :message "Invert negated condition: replace '(if (not <cond>) <then> <else>)' with '(if <cond> <else> <then>)'."
           :severity :style
           :suggested-fix replacement))))))

(defun default-cond-clause-p (test-node dialect)
  "Return T if TEST-NODE is a default cond clause branch (e.g. t, :else, otherwise)."
  (or (leaf-true-p test-node dialect)
      (leaf-symbol-p test-node "OTHERWISE")
      (leaf-symbol-p test-node ":ELSE")))

(defun single-clause-cond-info (node dialect)
  "If NODE is (cond (<test> <body...>)) with a non-default test, return (values T test-node body-nodes)."
  (let ((children (get-node-children node)))
    (when (and (compound-node-p node)
               (= (length children) 2)
               (leaf-symbol-p (first children) "COND"))
      (let* ((clause (second children))
             (clause-children (get-node-children clause)))
        (when (and (compound-node-p clause)
                   (>= (length clause-children) 2)
                   (not (default-cond-clause-p (first clause-children) dialect)))
          (values t (first clause-children) (rest clause-children)))))))

(defun check-single-clause-cond (node path dialect)
  "Detect (cond (<test> <body...>)) with a single clause."
  (multiple-value-bind (match-p test-node body-nodes)
      (single-clause-cond-info node dialect)
    (when match-p
      (let ((replacement (format nil "(when ~A ~{~A~^ ~})"
                                 (sexp-to-string test-node)
                                 (mapcar #'sexp-to-string body-nodes))))
        (make-lint-finding
         :rule :single-clause-cond
         :path path
         :message "'cond' with a single clause can be simplified to '(when <test> <body...>)'."
         :severity :style
         :suggested-fix replacement)))))

(defun check-if-boolean-redundant (node path dialect)
  "Detect (if <cond> t nil) or (if <cond> true false)."
  (let ((children (get-node-children node)))
    (when (and (compound-node-p node)
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
    (when (and (compound-node-p node)
               (= (length children) 2)
               (leaf-symbol-p (first children) "PROGN"))
      (let ((single-node (second children)))
        (make-lint-finding
         :rule :redundant-progn
         :path path
         :message "Single-expression 'progn' is redundant."
         :severity :style
         :suggested-fix (sexp-to-string single-node))))))

(defun let-form-parts (node)
  "If NODE is a compound (let bindings body...), return (values bindings body); otherwise (values nil nil)."
  (let ((children (get-node-children node)))
    (when (and (compound-node-p node)
               (>= (length children) 3)
               (leaf-symbol-p (first children) "LET"))
      (values (second children) (nthcdr 2 children)))))

(defun nested-let-info (node)
  "If NODE is (let outer-bindings (let inner-bindings ...)), return (values T outer-bindings inner-bindings inner-body)."
  (multiple-value-bind (outer-bindings outer-body) (let-form-parts node)
    (when (and (compound-node-p outer-bindings)
               (= (length outer-body) 1))
      (multiple-value-bind (inner-bindings inner-body) (let-form-parts (first outer-body))
        (when (compound-node-p inner-bindings)
          (values t outer-bindings inner-bindings inner-body))))))

(defun check-nested-let (node path dialect)
  "Detect nested (let ((x ...)) (let ((y ...)) ...)) that could be combined into let*."
  (declare (ignore dialect))
  (multiple-value-bind (match-p outer-bindings inner-bindings inner-body)
      (nested-let-info node)
    (when match-p
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
         :suggested-fix replacement)))))

(defun nil-comparison-form-p (node)
  "Return (values is-match-p other-node) if NODE is an (equal/eq/eql ?x nil) or (equal/eq/eql nil ?x) form."
  (let ((children (get-node-children node)))
    (when (and (compound-node-p node)
               (= (length children) 3)
               (or (leaf-symbol-p (first children) "EQUAL")
                   (leaf-symbol-p (first children) "EQ")
                   (leaf-symbol-p (first children) "EQL")))
      (cond
        ((leaf-nil-p (third children)) (values t (second children)))
        ((leaf-nil-p (second children)) (values t (third children)))
        (t (values nil nil))))))

(defun check-equal-nil-to-null (node path dialect)
  "Detect (equal ?x nil), (eq ?x nil), or (eql ?x nil) in Common Lisp / Elisp."
  (when (member dialect '(:common-lisp :emacs-lisp nil))
    (multiple-value-bind (match-p target-node) (nil-comparison-form-p node)
      (when match-p
        (let* ((target-str (sexp-to-string target-node))
               (replacement (format nil "(null ~A)" target-str)))
          (make-lint-finding
           :rule :equal-nil-to-null
           :path path
           :message (format nil "Prefer '(null ~A)' over comparison with nil." target-str)
           :severity :style
           :suggested-fix replacement))))))

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
  (let* ((start-node (resolve-tree-scope tree path))
         (findings '()))
    (when start-node
      (labels ((walk (node current-dialect)
                 (let* ((tag (get-node-tag node))
                        (node-path (get-node-path node))
                        (effective-dialect
                          (cond
                            ((supported-dialect-p tag)
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

(defun format-lint-fix (stream fix)
  "Format suggested fix FIX to STREAM."
  (when fix
    (cond
      ((and (listp fix) (getf fix :pattern) (getf fix :replacement))
       (format stream "   Suggested Fix:~%     Pattern:     ~A~%     Replacement: ~A~%"
               (getf fix :pattern) (getf fix :replacement)))
      ((stringp fix)
       (format stream "   Suggested Fix: ~A~%" fix)))))

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
              (format-lint-fix s fix)
              (format s "~%")))))

(defstruct (complexity-metrics (:constructor make-complexity-metrics))
  name
  kind
  path
  cyclomatic-complexity
  max-nesting-depth
  form-count
  recommendations)

(defun scheme-define-info (name-node)
  "Extract (values name-str kind) for a Scheme/Lisp 'define' form given its NAME-NODE."
  (if (compound-node-p name-node)
      (let ((fn-head (first (get-node-children name-node))))
        (values (format-atom (nth-value 2 (parse-node fn-head))) :function))
      (values (format-atom (nth-value 2 (parse-node name-node))) :definition)))

(defun standard-definition-kind (head-name)
  "Map HEAD-NAME to :function, :macro, :method, or :generic."
  (cond
    ((member head-name '("DEFUN" "DEFN" "DEFN-") :test #'string=) :function)
    ((member head-name '("DEFMACRO" "DEFSYNTAX") :test #'string=) :macro)
    ((string= head-name "DEFMETHOD") :method)
    ((string= head-name "DEFGENERIC") :generic)
    (t nil)))

(defun form-definition-info (node)
  "If NODE is a definition (defun, defmacro, defmethod, defgeneric, defn, define),
return (values is-def-p name-str kind-keyword)."
  (let ((children (get-node-children node)))
    (when (and (compound-node-p node)
               (>= (length children) 2))
      (multiple-value-bind (path tag val) (parse-node (first children))
        (declare (ignore path tag))
        (when (symbolp val)
          (let* ((head-name (string-upcase (symbol-name val)))
                 (name-node (second children))
                 (kind (standard-definition-kind head-name)))
            (cond
              (kind
               (values t (format-atom (nth-value 2 (parse-node name-node))) kind))
              ((string= head-name "DEFINE")
               (multiple-value-bind (name def-kind) (scheme-define-info name-node)
                 (values t name def-kind)))
              (t (values nil nil nil)))))))))

(defun count-cond-branch-clauses (clauses dialect)
  "Count non-default test clauses in COND form."
  (loop for clause in clauses
        for c-children = (get-node-children clause)
        when (and (compound-node-p clause) c-children)
          count (not (default-cond-clause-p (first c-children) dialect))))

(defun count-case-branch-clauses (clauses dialect)
  "Count non-default selector clauses in CASE forms."
  (loop for clause in clauses
        for c-children = (get-node-children clause)
        when (and (compound-node-p clause) c-children)
          count (not (or (leaf-true-p (first c-children) dialect)
                         (leaf-symbol-p (first c-children) "OTHERWISE")))))

(defun branch-form-complexity-increment (name children dialect)
  "Calculate McCabe complexity increment contributed by form NAME."
  (cond
    ((member name '("IF" "WHEN" "UNLESS" "WHEN-NOT" "IF-NOT" "WHEN-LET" "IF-LET" "WHEN-FIRST")
             :test #'string=)
     1)
    ((string= name "COND")
     (count-cond-branch-clauses (rest children) dialect))
    ((member name '("CASE" "CCASE" "ECASE" "TYPECASE" "CTYPECASE" "ETYPECASE" "CONDP")
             :test #'string=)
     (count-case-branch-clauses (nthcdr 2 children) dialect))
    ((member name '("AND" "OR") :test #'string=)
     (if (> (length children) 2) (- (length children) 2) 0))
    ((member name '("LOOP" "DOLIST" "DOTIMES" "DO" "DO*" "DOSEQ" "RECUR")
             :test #'string=)
     1)
    ((member name '("HANDLER-CASE" "RESTART-CASE") :test #'string=)
     (count-if #'compound-node-p (nthcdr 2 children)))
    (t 0)))

(defun node-operator-symbol-name (node)
  "If NODE is a compound form with a symbol head, return its uppercase symbol-name."
  (when (compound-node-p node)
    (let ((children (get-node-children node)))
      (when children
        (multiple-value-bind (p t-val val) (parse-node (first children))
          (declare (ignore p t-val))
          (when (symbolp val)
            (string-upcase (symbol-name val))))))))

(defun compute-branch-complexity (node &optional (dialect :common-lisp))
  "Compute McCabe cyclomatic complexity of NODE.
Base complexity is 1, with +1 for each conditional branch, short-circuit point, loop, or handler."
  (let ((complexity 1))
    (labels ((walk (curr)
               (let ((op (node-operator-symbol-name curr)))
                 (when op
                   (incf complexity
                         (branch-form-complexity-increment op (get-node-children curr) dialect)))
                 (dolist (c (get-node-children curr))
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

(defun collect-file-forms (file-node f-path)
  "Collect all (form-node . form-path) pairs under FILE-NODE."
  (loop for form in (get-node-children file-node)
        for form-idx from 0
        for form-path = (or (get-node-path form) (append f-path (list form-idx)))
        collect (cons form form-path)))

(defun collect-dialect-forms (dialect-node d-path)
  "Collect all (form-node . form-path) pairs under DIALECT-NODE."
  (loop for file-child in (get-node-children dialect-node)
        for f-idx from 0
        for f-path = (or (get-node-path file-child) (append d-path (list f-idx)))
        append (collect-file-forms file-child f-path)))

(defun collect-top-level-forms (node base-path)
  "Collect all (form-node . path) pairs from NODE (workspace, dialect, file, or single form)."
  (let ((tag (get-node-tag node)))
    (cond
      ((eq tag :workspace)
       (loop for dialect-child in (get-node-children node)
             for d-idx from 0
             for d-path = (or (get-node-path dialect-child) (append base-path (list d-idx)))
             append (collect-dialect-forms dialect-child d-path)))
      ((supported-dialect-p tag)
       (collect-dialect-forms node base-path))
      ((eq tag :file)
       (collect-file-forms node base-path))
      (t
       (list (cons node (or (get-node-path node) base-path)))))))

(defun compare-complexity-metrics (a b)
  "Sort comparator ordering COMPLEXITY-METRICS by cyclomatic complexity then max nesting depth."
  (let ((ca (complexity-metrics-cyclomatic-complexity a))
        (cb (complexity-metrics-cyclomatic-complexity b)))
    (if (= ca cb)
        (> (complexity-metrics-max-nesting-depth a)
           (complexity-metrics-max-nesting-depth b))
        (> ca cb))))

(defun analyze-complexity (tree &key path dialect (min-complexity 1) (min-depth 1))
  "Analyze structural complexity for forms in TREE (or under PATH).
Filters results to those meeting MIN-COMPLEXITY and MIN-DEPTH thresholds."
  (let* ((start-node (resolve-tree-scope tree path))
         (forms-with-paths (when start-node (collect-top-level-forms start-node path)))
         (min-cc (or min-complexity 1))
         (min-d (or min-depth 1))
         (results '()))
    (dolist (pair forms-with-paths)
      (let ((metrics (analyze-form-complexity (car pair) :path (cdr pair) :dialect dialect)))
        (when (and (>= (complexity-metrics-cyclomatic-complexity metrics) min-cc)
                   (>= (complexity-metrics-max-nesting-depth metrics) min-d))
          (push metrics results))))
    (sort results #'compare-complexity-metrics)))

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

(defstruct (duplicate-group (:constructor make-duplicate-group))
  code-snippet
  occurrence-count
  paths
  node-count
  depth
  recommendation)

(defun canonicalize-subtree (node &key (exact t))
  "Produce a canonical string fingerprint of NODE for equality/clone matching."
  (if exact
      (let* ((raw (sexp-to-string node))
             (cleaned (string-trim '(#\Space #\Newline #\Tab) raw)))
        cleaned)
      (labels ((anonymize (curr is-head)
                 (match curr
                   ((leaf path val)
                    (declare (ignore path))
                    (if is-head
                        curr
                        (list :path nil :leaf '?_)))
                   ((node path tag children)
                    (list* :path path tag
                           (loop for c in children
                                 for i from 0
                                 collect (anonymize c (zerop i)))))
                   (_ curr))))
        (sexp-to-string (anonymize node t)))))

(defun path-prefix-p (prefix path)
  "Return T if PREFIX is a strict prefix of PATH."
  (and (< (length prefix) (length path))
       (every #'= prefix (subseq path 0 (length prefix)))))

(defun same-top-level-form-p (paths)
  "Return T if all PATHS share the same parent form (e.g. within the same function)."
  (when (and paths (cdr paths))
    (let ((first-parent (if (<= (length (first paths)) 3)
                            (first paths)
                            (subseq (first paths) 0 3))))
      (every (lambda (p)
               (let ((parent (if (<= (length p) 3) p (subseq p 0 3))))
                 (equal first-parent parent)))
             (rest paths)))))

(defparameter *compiler-directive-heads*
  '("DECLARE" "DECLAIM" "PROCLAIM" "IN-PACKAGE" "DEFPACKAGE"
    "OPTIMIZE" "SPEED" "SAFETY" "SPACE" "COMPILATION-SPEED"
    "DEBUG" "INLINE" "NOTINLINE")
  "List of compiler directive, package, and declaration symbol names to ignore in clone detection.")

(defparameter *binding-form-heads*
  '("LET" "LET*" "FLET" "LABELS" "MACROLET" "SYMBOL-MACROLET"
    "WHEN-LET" "IF-LET" "WHEN-SOME" "IF-SOME" "DO" "DO*")
  "List of head symbols whose child 1 is a list of lexical bindings.")

(defun duplicate-subtree-eligible-p (tag children head-str context)
  "Return T if a node with TAG, CHILDREN, HEAD-STR, and CONTEXT is eligible for duplicate tracking."
  (and (member tag '(:paren :square :curly))
       children
       (not (member tag '(:workspace :common-lisp :clojure :scheme :emacs-lisp :fennel)))
       (not (member context '(:binding-list :binding-clause)))
       (not (member head-str *compiler-directive-heads* :test #'string=))))

(defun record-duplicate-subtree (curr curr-path node-cnt depth exact buckets node-metadata)
  "Record an eligible duplicate subtree node in BUCKETS and NODE-METADATA."
  (let ((fingerprint (canonicalize-subtree curr :exact exact)))
    (push (cons curr-path curr) (gethash fingerprint buckets nil))
    (unless (gethash fingerprint node-metadata)
      (setf (gethash fingerprint node-metadata)
            (list :node-count node-cnt :depth depth :sample curr)))))

(defun loop-with-vector-bindings-p (head-str children)
  "Return T if form is a LOOP with a vector binding clause."
  (and (equal head-str "LOOP")
       children
       (second children)
       (eq (get-node-tag (second children)) :square)))

(defun harvest-child-context (parent-context parent-tag head-str idx children)
  "Determine child context during harvest traversal."
  (cond
    ((and (eq parent-context :binding-list) (eq parent-tag :square))
     (if (evenp idx) :binding-clause nil))
    ((eq parent-context :binding-list)
     :binding-clause)
    ((eq parent-context :binding-clause)
     nil)
    ((or (member head-str *binding-form-heads* :test #'string=)
         (member head-str '("MULTIPLE-VALUE-BIND" "DESTRUCTURING-BIND") :test #'string=)
         (loop-with-vector-bindings-p head-str children))
     (if (= idx 1) :binding-list nil))
    (t nil)))

(defun harvest-duplicate-children (curr-path children tag head-str context exact min-nodes min-depth buckets node-metadata)
  "Traverse children of a node with contextual rules."
  (let ((skip-first (eq context :binding-clause)))
    (loop for child in (if skip-first (rest children) children)
          for idx from (if skip-first 1 0)
          for cp = (or (get-node-path child) (append curr-path (list idx)))
          for c-ctx = (harvest-child-context context tag head-str idx children)
          do (harvest-duplicate-subtrees child cp exact min-nodes min-depth buckets node-metadata c-ctx))))

(defun maybe-record-duplicate-subtree (node curr-path exact min-nodes min-depth buckets node-metadata context head-str)
  "Check eligibility and record candidate duplicate subtree."
  (when (duplicate-subtree-eligible-p (get-node-tag node) (get-node-children node) head-str context)
    (let ((node-cnt (count-ast-nodes node))
          (depth (compute-nesting-depth node 0)))
      (when (and (>= node-cnt (or min-nodes 4))
                 (>= depth (or min-depth 2)))
        (record-duplicate-subtree node curr-path node-cnt depth exact buckets node-metadata)))))

(defun harvest-duplicate-subtrees (node curr-path exact min-nodes min-depth buckets node-metadata &optional context)
  "Recursively traverse NODE to collect candidate subtrees into BUCKETS and NODE-METADATA."
  (let* ((children (get-node-children node))
         (first-child (first children))
         (first-val (when (and first-child (eq (get-node-tag first-child) :leaf))
                      (nth-value 2 (parse-node first-child))))
         (head-str (when (symbolp first-val) (string-upcase (symbol-name first-val)))))
    (maybe-record-duplicate-subtree node curr-path exact min-nodes min-depth buckets node-metadata context head-str)
    (unless (member head-str '("DECLARE" "DECLAIM" "PROCLAIM" "IN-PACKAGE" "DEFPACKAGE") :test #'string=)
      (harvest-duplicate-children curr-path children (get-node-tag node)
                                  head-str context exact min-nodes min-depth buckets node-metadata))))

(defun extract-raw-duplicate-candidates (buckets node-metadata)
  "Extract entries appearing at least twice from BUCKETS and associate with NODE-METADATA."
  (let ((raw-candidates '()))
    (maphash
     (lambda (fingerprint entries)
       (when (>= (length entries) 2)
         (let* ((meta (gethash fingerprint node-metadata))
                (paths (mapcar #'car entries))
                (node-cnt (getf meta :node-count))
                (depth (getf meta :depth))
                (sample (getf meta :sample)))
           (push (list :fingerprint fingerprint
                       :paths (nreverse paths)
                       :node-count node-cnt
                       :depth depth
                       :sample sample)
                 raw-candidates))))
     buckets)
    raw-candidates))

(defun candidate-subsumed-p (cand candidates)
  "Return T if CAND is subsumed by any other candidate in CANDIDATES."
  (let* ((c-paths (getf cand :paths))
         (c-count (length c-paths)))
    (some (lambda (other)
            (unless (eq cand other)
              (let ((o-paths (getf other :paths))
                    (o-count (length (getf other :paths))))
                (and (= c-count o-count)
                     (> (getf other :node-count) (getf cand :node-count))
                     (every (lambda (cp)
                              (some (lambda (op) (path-prefix-p op cp)) o-paths))
                            c-paths)))))
          candidates)))

(defun filter-subsumed-candidates (candidates)
  "Filter out candidates that are completely subsumed by larger subtrees with identical occurrences."
  (remove-if (lambda (cand) (candidate-subsumed-p cand candidates)) candidates))

(defun make-duplicate-snippet (sample)
  "Generate a single-line truncated snippet for SAMPLE node."
  (let* ((raw-str (sexp-to-string sample))
         (single-line (substitute #\Space #\Newline (string-trim '(#\Space #\Newline #\Tab) raw-str))))
    (if (> (length single-line) 80)
        (format nil "~A..." (subseq single-line 0 77))
        single-line)))

(defun build-duplicate-group (cand)
  "Build a DUPLICATE-GROUP instance from CAND."
  (let* ((paths (getf cand :paths))
         (sample (getf cand :sample))
         (snippet (make-duplicate-snippet sample))
         (node-cnt (getf cand :node-count))
         (depth (getf cand :depth))
         (recomm (if (same-top-level-form-p paths)
                     "Repeated expression within the same function — consider extracting into a local variable using 'ast_extract_variable'."
                     "Repeated code across multiple locations — consider extracting into a shared helper function using 'ast_extract_function'.")))
    (make-duplicate-group
     :code-snippet snippet
     :occurrence-count (length paths)
     :paths paths
     :node-count node-cnt
     :depth depth
     :recommendation recomm)))

(defun sort-duplicate-groups (groups)
  "Sort duplicate groups descending by AST savings, then by occurrence count."
  (sort groups
        (lambda (a b)
          (let ((savings-a (* (duplicate-group-node-count a) (1- (duplicate-group-occurrence-count a))))
                (savings-b (* (duplicate-group-node-count b) (1- (duplicate-group-occurrence-count b)))))
            (if (= savings-a savings-b)
                (> (duplicate-group-occurrence-count a) (duplicate-group-occurrence-count b))
                (> savings-a savings-b))))))

(defun find-duplicate-subtrees (tree &key path (min-nodes 4) (min-depth 2) (exact t))
  "Find repeated AST subtrees in TREE (or under PATH).
Groups matching subtrees, removes redundant subsumed sub-expressions, and generates refactoring recommendations."
  (let ((start-node (resolve-tree-scope tree path)))
    (unless start-node
      (return-from find-duplicate-subtrees nil))
    (let ((buckets (make-hash-table :test 'equal))
          (node-metadata (make-hash-table :test 'equal))
          (start-path (or (get-node-path start-node) path '())))
      (harvest-duplicate-subtrees start-node start-path exact min-nodes min-depth buckets node-metadata)
      (let* ((raw-candidates (extract-raw-duplicate-candidates buckets node-metadata))
             (filtered-candidates (filter-subsumed-candidates raw-candidates))
             (groups (mapcar #'build-duplicate-group filtered-candidates)))
        (sort-duplicate-groups groups)))))

(defun format-duplicate-report (duplicate-groups)
  "Format a list of DUPLICATE-GROUP instances into a readable diagnostic report."
  (if (null duplicate-groups)
      "No duplicate subtrees or structural clones detected."
      (with-output-to-string (s)
        (format s "Duplicate Subtrees Report (~A duplicate group~:P found):~%~%" (length duplicate-groups))
        (loop for g in duplicate-groups
              for i from 1
              for snippet = (duplicate-group-code-snippet g)
              for count = (duplicate-group-occurrence-count g)
              for paths = (duplicate-group-paths g)
              for node-cnt = (duplicate-group-node-count g)
              for depth = (duplicate-group-depth g)
              for rec = (duplicate-group-recommendation g)
              do
              (format s "~A. [~A occurrences | ~A nodes | depth ~A]~%   Code: ~A~%   Paths:~%"
                      i count node-cnt depth snippet)
              (dolist (p paths)
                (format s "     - [~{~A~^, ~}]~%" p))
              (when rec
                (format s "   Recommendation: ~A~%" rec))
              (format s "~%")))))

(defstruct binding-finding
  kind
  variable-name
  path
  scope-kind
  outer-path
  message
  recommendation)

(defstruct scope-binding
  name
  path
  scope-kind
  enclosing-name
  (ignored-p nil)
  (usage-count 0)
  (usage-paths nil))

(defstruct lexical-scope
  kind
  parent
  dialect
  enclosing-name
  (bindings nil))

(defparameter *cl-lambda-keywords*
  '("&optional" "&rest" "&key" "&aux" "&body" "&whole" "&environment" "&allow-other-keys" "&"))

(defparameter *lisp-special-operators*
  '("if" "when" "unless" "cond" "case" "typecase" "ecase" "ccase" "ctypecase" "etypecase"
    "condp" "and" "or" "do" "progn" "block" "return-from" "tagbody" "go" "catch" "throw"
    "unwind-protect" "let" "let*" "letrec" "loop" "defun" "defmacro" "defmethod" "defgeneric"
    "define" "defn" "defn-" "fn" "lambda" "quote" "'" "function" "#'" "declare"
    "multiple-value-bind" "destructuring-bind" "dolist" "dotimes" "when-let" "if-let" "when-some" "if-some"))

(defun lisp-1-dialect-p (dialect)
  (member dialect '(:clojure :scheme :fennel)))

(defun vector-binding-dialect-p (dialect)
  "Return T if DIALECT uses vector [var val ...] bindings (Clojure and Fennel)."
  (or (eq dialect :clojure) (eq dialect :fennel)))

(defun leaf-any-symbol-p (node)
  "Return T if NODE is a leaf node containing a symbol (not keyword, boolean, or literal)."
  (multiple-value-bind (path tag val) (parse-node node)
    (declare (ignore path))
    (and (eq tag :leaf)
         (symbolp val)
         (not (keywordp val))
         (not (member val '(t nil))))))

(defun leaf-symbol-name (node)
  "Return downcased string name of leaf symbol node, or NIL if not a leaf symbol."
  (multiple-value-bind (path tag val) (parse-node node)
    (declare (ignore path))
    (when (and (eq tag :leaf) (symbolp val))
      (string-downcase (symbol-name val)))))

(defun ignored-variable-name-p (name)
  "Return T if variable NAME follows ignored naming conventions (_ or starts with _)."
  (or (string= name "_")
      (starts-with-subseq "_" name)
      (string-equal name "ignore")
      (string-equal name "unused")))

(defun dynamic-variable-name-p (name)
  "Return T if variable NAME has earmuffs (*...*), indicating a dynamic variable."
  (and (>= (length name) 2)
       (char= (char name 0) #\*)
       (char= (char name (1- (length name))) #\*)))

(defun extract-spec-ignored-vars (spec)
  "Extract ignored/ignorable variable names from a single declaration specifier form SPEC."
  (let ((result '()))
    (when (eq (get-node-tag spec) :paren)
      (let* ((spec-children (get-node-children spec))
             (spec-name (when spec-children (leaf-symbol-name (first spec-children)))))
        (when (member spec-name '("ignore" "ignorable") :test #'string=)
          (dolist (var-node (rest spec-children))
            (when (leaf-any-symbol-p var-node)
              (push (leaf-symbol-name var-node) result))))))
    result))

(defun extract-cl-declarations (body-nodes)
  "Extract ignored/ignorable variable names from leading (declare ...) forms in BODY-NODES.
Returns (values ignored-names remaining-body-nodes)."
  (let ((ignored '())
        (remaining body-nodes))
    (loop while remaining
          for form = (first remaining)
          for tag = (get-node-tag form)
          for children = (get-node-children form)
          while (and (eq tag :paren)
                     children
                     (equal (leaf-symbol-name (first children)) "declare"))
          do
          (dolist (spec (rest children))
            (dolist (var (extract-spec-ignored-vars spec))
              (push var ignored)))
          (setf remaining (rest remaining)))
    (values ignored remaining)))

(defun get-scope-declarations (body-nodes dialect)
  "Extract ignored/ignorable variable declarations from leading forms in BODY-NODES.
Returns (values ignored-names remaining-body-nodes)."
  (if (lisp-1-dialect-p dialect)
      (values nil body-nodes)
      (extract-cl-declarations body-nodes)))

(defun find-in-lexical-scope (scope var-name)
  "Look up VAR-NAME in SCOPE and its enclosing parent scopes. Returns scope-binding or NIL."
  (when scope
    (or (find var-name (lexical-scope-bindings scope)
              :key #'scope-binding-name
              :test #'string=)
        (find-in-lexical-scope (lexical-scope-parent scope) var-name))))

(defun register-scope-binding (scope name path &key (ignored nil))
  "Register a new binding in SCOPE. Returns the new scope-binding."
  (let* ((is-ignored (or ignored (ignored-variable-name-p name)))
         (binding (make-scope-binding
                   :name name
                   :path path
                   :scope-kind (lexical-scope-kind scope)
                   :enclosing-name (lexical-scope-enclosing-name scope)
                   :ignored-p is-ignored)))
    (push binding (lexical-scope-bindings scope))
    binding))

(defun record-variable-usage (scope var-name usage-path)
  "Record an occurrence of VAR-NAME at USAGE-PATH in the nearest enclosing binding."
  (let ((binding (find-in-lexical-scope scope var-name)))
    (when binding
      (incf (scope-binding-usage-count binding))
      (push usage-path (scope-binding-usage-paths binding))
      t)))

(defun collect-cl-spec-param (children collect-fn)
  "Collect parameter bindings from a Common Lisp optional or keyword spec."
  (let ((first-child (first children)))
    (if (and (eq (get-node-tag first-child) :paren)
             (get-node-children first-child))
        (let ((sub (get-node-children first-child)))
          (when (>= (length sub) 2)
            (funcall collect-fn (second sub))))
        (funcall collect-fn first-child))
    (when (>= (length children) 3)
      (funcall collect-fn (third children)))))

(defun cl-param-spec-p (tag first-child)
  "Check if FIRST-CHILD under a collection of TAG represents a CL param spec."
  (and (eq tag :paren)
       (or (leaf-any-symbol-p first-child)
           (and (eq (get-node-tag first-child) :paren)
                (get-node-children first-child)))))

(defun collect-sequence-params (tag children collect-fn)
  "Collect parameter bindings from paren or square sequence CHILDREN."
  (when children
    (if (cl-param-spec-p tag (first children))
        (collect-cl-spec-param children collect-fn)
        (dolist (c children)
          (funcall collect-fn c)))))

(defun collect-map-destructuring-entry (k v collect-fn)
  "Collect bindings from a single map destructuring pair (K V)."
  (let ((k-name (leaf-symbol-name k)))
    (cond
      ((and (equal k-name ":keys") (member (get-node-tag v) '(:square :paren)))
       (dolist (c (get-node-children v))
         (funcall collect-fn c)))
      ((and (equal k-name ":as") (leaf-any-symbol-p v))
       (funcall collect-fn v))
      ((leaf-any-symbol-p k)
       (funcall collect-fn k)))))

(defun collect-map-destructuring-params (children collect-fn)
  "Collect bindings from curly map destructuring CHILDREN."
  (loop for (k v) on children by #'cddr
        while k
        do (collect-map-destructuring-entry k v collect-fn)))

(defun collect-single-param (node collect-fn on-symbol-fn)
  "Dispatch parameter collection for a single parameter NODE."
  (when node
    (let ((tag (get-node-tag node)))
      (cond
        ((leaf-any-symbol-p node)
         (let ((name (leaf-symbol-name node)))
           (unless (member name *cl-lambda-keywords* :test #'string=)
             (funcall on-symbol-fn name (get-node-path node)))))
        ((member tag '(:paren :square))
         (collect-sequence-params tag (get-node-children node) collect-fn))
        ((eq tag :curly)
         (collect-map-destructuring-params (get-node-children node) collect-fn))))))

(defun extract-param-bindings (params-node)
  "Extract list of (name . path) pairs from PARAMS-NODE."
  (let ((results '()))
    (labels ((collect (node)
               (collect-single-param
                node
                #'collect
                (lambda (name path) (push (cons name path) results)))))
      (if (compound-node-p params-node)
          (dolist (c (get-node-children params-node))
            (collect c))
          (collect params-node)))
    (nreverse results)))

(defun extract-single-let-clause (clause)
  "Parse a single CLAUSE into plist (:pattern node :init node)."
  (cond
    ((leaf-any-symbol-p clause)
     (list :pattern clause :init nil))
    ((compound-node-p clause)
     (let ((c-children (get-node-children clause)))
       (list :pattern (first c-children)
             :init (second c-children))))
    (t nil)))

(defun extract-let-clauses (bindings-node dialect)
  "Extract list of plist (:pattern node :init node) from BINDINGS-NODE according to DIALECT."
  (unless bindings-node (return-from extract-let-clauses nil))
  (let ((tag (get-node-tag bindings-node))
        (children (get-node-children bindings-node)))
    (if (or (eq dialect :clojure) (eq dialect :fennel) (eq tag :square))
        (loop for (pat-node init-node) on children by #'cddr
              while pat-node
              collect (list :pattern pat-node :init init-node))
        (loop for clause in children
              for parsed = (extract-single-let-clause clause)
              when parsed collect parsed))))

(defun check-and-register-binding (scope name path ignored-names findings)
  (let ((outer (find-in-lexical-scope (lexical-scope-parent scope) name))
        (ignored (member name ignored-names :test #'string-equal)))
    (when (and outer
               (not (ignored-variable-name-p name))
               (not (dynamic-variable-name-p name)))
      (push (make-binding-finding
             :kind :shadowed-variable
             :variable-name name
             :path path
             :scope-kind (lexical-scope-kind scope)
             :outer-path (scope-binding-path outer)
             :message (format nil "Variable '~A' in ~A shadows outer binding at [~{~A~^, ~}]."
                              name (lexical-scope-kind scope) (scope-binding-path outer))
             :recommendation (format nil "Consider renaming local variable '~A' using 'ast_rename' to avoid shadowing." name))
            findings))
    (register-scope-binding scope name path :ignored (or ignored (ignored-variable-name-p name)))
    findings))

(defun check-unused-in-scope (scope dialect findings)
  (dolist (b (lexical-scope-bindings scope))
    (when (and (= (scope-binding-usage-count b) 0)
               (not (scope-binding-ignored-p b)))
      (let* ((name (scope-binding-name b))
             (s-kind (scope-binding-scope-kind b))
             (path (scope-binding-path b))
             (recomm
               (if (member s-kind '(:function :macro :lambda :method :definition))
                   (if (lisp-1-dialect-p dialect)
                       (format nil "If intentionally unused, prefix with '_' (e.g. '_~A')." name)
                       (format nil "If intentionally unused, prefix with '_' or add '(declare (ignore ~A))'." name))
                   (format nil "Variable '~A' is unused. Consider removing it with 'ast_remove' or prefixing with '_'." name))))
        (push (make-binding-finding
               :kind :unused-variable
               :variable-name name
               :path path
               :scope-kind s-kind
               :outer-path nil
               :message (format nil "Variable '~A' defined in ~A is never used." name s-kind)
               :recommendation recomm)
              findings))))
  findings)

(defun walk-binding-nodes (nodes scope dialect findings-acc)
  "Walk a list of NODES in SCOPE, accumulating binding findings."
  (dolist (item nodes findings-acc)
    (setf findings-acc (walk-binding-tree item scope dialect findings-acc))))

(defun extract-scheme-define (name-child children)
  "Extract define components for Scheme-style (define (name . params) body)."
  (let ((sig-children (get-node-children name-child)))
    (when sig-children
      (values (leaf-symbol-name (first sig-children))
              (list* :path (get-node-path name-child) :paren (rest sig-children))
              (cddr children)))))

(defun extract-clojure-defn (children)
  "Extract defn components for Clojure defn/defn-."
  (let ((rem (cddr children)))
    (when (and rem (or (stringp (third (parse-node (first rem))))
                       (eq (get-node-tag (first rem)) :curly)))
      (setf rem (rest rem)))
    (if (and rem (eq (get-node-tag (first rem)) :square))
        (values (first rem) (rest rem))
        (values nil rem))))

(defun extract-defun-components (head-name children dialect)
  "Extract (values fn-name params-node body-nodes) from a defun-like form."
  (let* ((name-child (second children))
         (fn-name (if (leaf-any-symbol-p name-child) (leaf-symbol-name name-child) "anonymous")))
    (cond
      ((and (equal head-name "define") (compound-node-p name-child))
       (multiple-value-bind (name params body)
           (extract-scheme-define name-child children)
         (values (or name fn-name) params body)))
      ((or (eq dialect :clojure) (member head-name '("defn" "defn-") :test #'string=))
       (multiple-value-bind (params body)
           (extract-clojure-defn children)
         (values fn-name params body)))
      (t
       (values fn-name (third children) (cdddr children))))))

(defun walk-single-fn-method (fn-name params-node body-nodes scope dialect findings-acc)
  "Walk a single function or method definition body."
  (multiple-value-bind (ignored rem-body)
      (get-scope-declarations body-nodes dialect)
    (let* ((fn-scope (make-lexical-scope :kind :function
                                         :parent scope
                                         :dialect dialect
                                         :enclosing-name fn-name))
           (params (extract-param-bindings params-node)))
      (dolist (p params)
        (setf findings-acc (check-and-register-binding fn-scope (car p) (cdr p) ignored findings-acc)))
      (setf findings-acc (walk-binding-nodes rem-body fn-scope dialect findings-acc))
      (check-unused-in-scope fn-scope dialect findings-acc))))

(defun walk-multi-arity-fn (fn-name body-nodes scope dialect findings-acc)
  "Walk multi-arity function bodies (e.g. Clojure or Scheme)."
  (dolist (form body-nodes findings-acc)
    (if (compound-node-p form)
        (let* ((f-children (get-node-children form))
               (p-node (first f-children))
               (b-nodes (rest f-children)))
          (if (and p-node (compound-node-p p-node))
              (setf findings-acc (walk-single-fn-method fn-name p-node b-nodes scope dialect findings-acc))
              (setf findings-acc (walk-binding-tree form scope dialect findings-acc))))
        (setf findings-acc (walk-binding-tree form scope dialect findings-acc)))))

(defun walk-defun-binding-form (head-name children scope dialect findings-acc)
  "Walk defun, defmacro, defmethod, defn, or define form."
  (multiple-value-bind (fn-name params-node body-nodes)
      (extract-defun-components head-name children dialect)
    (if params-node
        (walk-single-fn-method fn-name params-node body-nodes scope dialect findings-acc)
        (walk-multi-arity-fn fn-name body-nodes scope dialect findings-acc))))

(defun extract-lambda-params-and-body (children)
  "Extract (values params-node body-nodes) from lambda or fn children."
  (let ((rem (rest children)))
    (when (and rem (leaf-any-symbol-p (first rem)) (rest rem))
      (setf rem (rest rem)))
    (when rem
      (values (first rem) (rest rem)))))

(defun walk-lambda-body (params-node body-nodes scope dialect findings-acc)
  "Walk the parameter and body nodes of a lambda form."
  (multiple-value-bind (ignored rem-body)
      (get-scope-declarations body-nodes dialect)
    (let* ((lam-scope (make-lexical-scope :kind :lambda
                                          :parent scope
                                          :dialect dialect
                                          :enclosing-name "lambda"))
           (params (extract-param-bindings params-node)))
      (dolist (p params)
        (setf findings-acc (check-and-register-binding lam-scope (car p) (cdr p) ignored findings-acc)))
      (setf findings-acc (walk-binding-nodes rem-body lam-scope dialect findings-acc))
      (check-unused-in-scope lam-scope dialect findings-acc))))

(defun walk-lambda-binding-form (children scope dialect findings-acc)
  "Walk lambda or fn form."
  (multiple-value-bind (params-node body-nodes)
      (extract-lambda-params-and-body children)
    (if (and params-node (compound-node-p params-node))
        (walk-lambda-body params-node body-nodes scope dialect findings-acc)
        (walk-binding-nodes (rest children) scope dialect findings-acc))))

(defun walk-cl-let-form (children scope dialect findings-acc)
  "Walk parallel Common Lisp LET form."
  (let* ((bindings-node (second children))
         (body-nodes (cddr children))
         (clauses (extract-let-clauses bindings-node dialect)))
    (dolist (cl clauses)
      (when (getf cl :init)
        (setf findings-acc (walk-binding-tree (getf cl :init) scope dialect findings-acc))))
    (multiple-value-bind (ignored rem-body)
        (get-scope-declarations body-nodes dialect)
      (let ((let-scope (make-lexical-scope :kind :let :parent scope :dialect dialect)))
        (dolist (cl clauses)
          (let ((bound (extract-param-bindings (getf cl :pattern))))
            (dolist (p bound)
              (setf findings-acc (check-and-register-binding let-scope (car p) (cdr p) ignored findings-acc)))))
        (setf findings-acc (walk-binding-nodes rem-body let-scope dialect findings-acc))
        (check-unused-in-scope let-scope dialect findings-acc)))))

(defun walk-sequential-let-form (head-name children scope dialect findings-acc)
  "Walk LET*, LETREC, or vector-binding LET/LOOP forms."
  (let* ((bindings-node (second children))
         (body-nodes (cddr children))
         (clauses (extract-let-clauses bindings-node dialect))
         (curr-scope scope)
         (created-scopes '()))
    (multiple-value-bind (ignored rem-body)
        (get-scope-declarations body-nodes dialect)
      (dolist (cl clauses)
        (when (getf cl :init)
          (setf findings-acc (walk-binding-tree (getf cl :init) curr-scope dialect findings-acc)))
        (let ((step-scope (make-lexical-scope :kind (if (equal head-name "loop") :loop :let*)
                                              :parent curr-scope
                                              :dialect dialect))
              (bound (extract-param-bindings (getf cl :pattern))))
          (dolist (p bound)
            (setf findings-acc (check-and-register-binding step-scope (car p) (cdr p) ignored findings-acc)))
          (push step-scope created-scopes)
          (setf curr-scope step-scope)))
      (setf findings-acc (walk-binding-nodes rem-body curr-scope dialect findings-acc))
      (dolist (sc created-scopes findings-acc)
        (setf findings-acc (check-unused-in-scope sc dialect findings-acc))))))

(defun walk-binding-bind-form (kind pattern-child val-child body-children scope dialect findings-acc)
  "Walk MULTIPLE-VALUE-BIND or DESTRUCTURING-BIND form."
  (setf findings-acc (walk-binding-tree val-child scope dialect findings-acc))
  (multiple-value-bind (ignored rem-body)
      (get-scope-declarations body-children dialect)
    (let ((b-scope (make-lexical-scope :kind kind :parent scope :dialect dialect))
          (bound (extract-param-bindings pattern-child)))
      (dolist (p bound)
        (setf findings-acc (check-and-register-binding b-scope (car p) (cdr p) ignored findings-acc)))
      (setf findings-acc (walk-binding-nodes rem-body b-scope dialect findings-acc))
      (check-unused-in-scope b-scope dialect findings-acc))))

(defun walk-iteration-binding-form (head-name children scope dialect findings-acc)
  "Walk DOLIST or DOTIMES form."
  (let* ((spec-node (second children))
         (body-nodes (cddr children)))
    (if (and spec-node (compound-node-p spec-node))
        (let* ((spec-children (get-node-children spec-node))
               (var-node (first spec-children))
               (count-or-list (second spec-children))
               (res-form (third spec-children)))
          (when count-or-list
            (setf findings-acc (walk-binding-tree count-or-list scope dialect findings-acc)))
          (multiple-value-bind (ignored rem-body)
              (get-scope-declarations body-nodes dialect)
            (let ((loop-scope (make-lexical-scope :kind (if (equal head-name "dolist") :dolist :dotimes)
                                                  :parent scope
                                                  :dialect dialect)))
              (when (leaf-any-symbol-p var-node)
                (setf findings-acc (check-and-register-binding loop-scope
                                                               (leaf-symbol-name var-node)
                                                               (get-node-path var-node)
                                                               ignored
                                                               findings-acc)))
              (setf findings-acc (walk-binding-nodes rem-body loop-scope dialect findings-acc))
              (when res-form
                (setf findings-acc (walk-binding-tree res-form loop-scope dialect findings-acc)))
              (check-unused-in-scope loop-scope dialect findings-acc))))
        (walk-binding-nodes (rest children) scope dialect findings-acc))))

(defun walk-when-let-form (children scope dialect findings-acc)
  "Walk WHEN-LET, IF-LET, WHEN-SOME, or IF-SOME form."
  (let* ((bindings-node (second children))
         (body-nodes (cddr children))
         (clauses (extract-let-clauses bindings-node dialect))
         (wl-scope (make-lexical-scope :kind :when-let :parent scope :dialect dialect)))
    (dolist (cl clauses)
      (when (getf cl :init)
        (setf findings-acc (walk-binding-tree (getf cl :init) scope dialect findings-acc)))
      (let ((bound (extract-param-bindings (getf cl :pattern))))
        (dolist (p bound)
          (setf findings-acc (check-and-register-binding wl-scope (car p) (cdr p) nil findings-acc)))))
    (setf findings-acc (walk-binding-nodes body-nodes wl-scope dialect findings-acc))
    (check-unused-in-scope wl-scope dialect findings-acc)))

(defun walk-flet-labels-def (f-def scope dialect findings-acc)
  "Walk a single function definition inside FLET or LABELS."
  (when (and f-def (compound-node-p f-def))
    (let* ((f-children (get-node-children f-def))
           (fn-name-node (first f-children))
           (fn-name (if (leaf-any-symbol-p fn-name-node) (leaf-symbol-name fn-name-node) "local-fn"))
           (p-node (second f-children))
           (b-nodes (cddr f-children)))
      (when (and p-node (compound-node-p p-node))
        (multiple-value-bind (ignored rem-body)
            (get-scope-declarations b-nodes dialect)
          (let* ((loc-scope (make-lexical-scope :kind :function
                                                :parent scope
                                                :dialect dialect
                                                :enclosing-name fn-name))
                 (params (extract-param-bindings p-node)))
            (dolist (p params)
              (setf findings-acc (check-and-register-binding loc-scope (car p) (cdr p) ignored findings-acc)))
            (setf findings-acc (walk-binding-nodes rem-body loc-scope dialect findings-acc))
            (setf findings-acc (check-unused-in-scope loc-scope dialect findings-acc)))))))
  findings-acc)

(defun walk-flet-labels-form (children scope dialect findings-acc)
  "Walk FLET or LABELS form."
  (let* ((fns-node (second children))
         (body-nodes (cddr children)))
    (when (and fns-node (compound-node-p fns-node))
      (dolist (f-def (get-node-children fns-node))
        (setf findings-acc (walk-flet-labels-def f-def scope dialect findings-acc))))
    (walk-binding-nodes body-nodes scope dialect findings-acc)))

(defun binding-ignore-head-p (head-name dialect)
  "Return T if HEAD-NAME represents a non-binding form to ignore."
  (or (member head-name '("quote" "'" "declare") :test #'string=)
      (and (not (lisp-1-dialect-p dialect))
           (member head-name '("function" "#'") :test #'string=))))

(defun walk-function-definition-form (head-name children scope dialect findings-acc)
  "Walk a function or lambda definition form."
  (if (member head-name '("lambda" "fn") :test #'string=)
      (walk-lambda-binding-form children scope dialect findings-acc)
      (walk-defun-binding-form head-name children scope dialect findings-acc)))

(defun walk-fallback-binding-form (head-name children scope dialect findings-acc)
  "Record Lisp-1 variable usage if applicable, and walk remaining children."
  (let ((head (first children)))
    (when (and (lisp-1-dialect-p dialect)
               (leaf-any-symbol-p head)
               (not (member head-name *lisp-special-operators* :test #'string=)))
      (record-variable-usage scope head-name (get-node-path head))))
  (walk-binding-nodes (rest children) scope dialect findings-acc))

(defun sequential-binding-form-p (head-name dialect)
  "Return T if HEAD-NAME in DIALECT evaluates bindings sequentially."
  (or (member head-name '("let*" "letrec") :test #'string=)
      (and (member head-name '("let" "loop") :test #'string=)
           (vector-binding-dialect-p dialect))))

(defun walk-bind-form (head-name children scope dialect findings-acc)
  "Walk multiple-value-bind or destructuring-bind."
  (let ((kind (if (equal head-name "multiple-value-bind") :multiple-value-bind :destructuring-bind)))
    (walk-binding-bind-form kind (second children) (third children) (cdddr children)
                            scope dialect findings-acc)))

(defun walk-scope-binding-form (head-name children scope dialect findings-acc)
  "Walk lexical bindings, iteration, or fallback forms."
  (cond
    ((and (equal head-name "let") (not (vector-binding-dialect-p dialect)))
     (walk-cl-let-form children scope dialect findings-acc))
    ((sequential-binding-form-p head-name dialect)
     (walk-sequential-let-form head-name children scope dialect findings-acc))
    ((equal head-name "loop")
     (walk-binding-nodes (rest children) scope dialect findings-acc))
    ((member head-name '("multiple-value-bind" "destructuring-bind") :test #'string=)
     (walk-bind-form head-name children scope dialect findings-acc))
    ((member head-name '("dolist" "dotimes") :test #'string=)
     (walk-iteration-binding-form head-name children scope dialect findings-acc))
    ((member head-name '("when-let" "if-let" "when-some" "if-some") :test #'string=)
     (walk-when-let-form children scope dialect findings-acc))
    ((member head-name '("flet" "labels") :test #'string=)
     (walk-flet-labels-form children scope dialect findings-acc))
    (t
     (walk-fallback-binding-form head-name children scope dialect findings-acc))))

(defun walk-compound-binding-form (head-name children scope dialect findings-acc)
  "Dispatch binding analysis for compound forms by operator head name."
  (cond
    ((binding-ignore-head-p head-name dialect)
     findings-acc)
    ((member head-name '("defun" "defmacro" "defmethod" "defn" "defn-" "define" "lambda" "fn") :test #'string=)
     (walk-function-definition-form head-name children scope dialect findings-acc))
    (t
     (walk-scope-binding-form head-name children scope dialect findings-acc))))

(defun node-head-leaf-name (children)
  "Return symbol name of the first child leaf node if present."
  (when children
    (let ((head (first children)))
      (when (leaf-any-symbol-p head)
        (leaf-symbol-name head)))))

(defun walk-binding-tree (node scope dialect findings-acc)
  "Recursively walk NODE in SCOPE, updating findings accumulator and resolving variable usages."
  (cond
    ((null node)
     findings-acc)
    ((eq (get-node-tag node) :leaf)
     (when (leaf-any-symbol-p node)
       (record-variable-usage scope (leaf-symbol-name node) (get-node-path node)))
     findings-acc)
    ((eq (get-node-tag node) :comment)
     findings-acc)
    ((member (get-node-tag node) '(:paren :square))
     (let ((children (get-node-children node)))
       (if children
           (walk-compound-binding-form (node-head-leaf-name children) children scope dialect findings-acc)
           findings-acc)))
    (t
     (walk-binding-nodes (get-node-children node) scope dialect findings-acc))))

(defun analyze-bindings (tree &key path (include-unused t) (include-shadowed t) (dialect *current-dialect*))
  "Analyze variable bindings in TREE (or under PATH) for unused and shadowed variables.
Returns a list of BINDING-FINDING instances."
  (let* ((start-node (resolve-tree-scope tree path))
         (findings (walk-binding-tree start-node nil dialect '())))
    (setf findings (nreverse findings))
    (remove-if-not
     (lambda (f)
       (case (binding-finding-kind f)
         (:unused-variable include-unused)
         (:shadowed-variable include-shadowed)
         (t t)))
     findings)))

(defun format-finding-recommendation (stream f)
  "Format recommendation for binding finding F to STREAM if present."
  (when (binding-finding-recommendation f)
    (format stream "     Recommendation: ~A~%" (binding-finding-recommendation f))))

(defun format-unused-bindings (stream unused)
  "Format list of unused variable findings to STREAM."
  (when unused
    (format stream "Unused Variables (~A):~%" (length unused))
    (loop for f in unused
          for i from 1
          do
             (format stream "  ~A. '~A' in ~A [~{~A~^, ~}]~%"
                     i (binding-finding-variable-name f)
                     (binding-finding-scope-kind f)
                     (binding-finding-path f))
             (format-finding-recommendation stream f))
    (format stream "~%")))

(defun format-shadowed-bindings (stream shadowed)
  "Format list of shadowed variable findings to STREAM."
  (when shadowed
    (format stream "Shadowed Variables (~A):~%" (length shadowed))
    (loop for f in shadowed
          for i from 1
          do
             (format stream "  ~A. '~A' in ~A [~{~A~^, ~}] shadows outer binding at [~{~A~^, ~}]~%"
                     i (binding-finding-variable-name f)
                     (binding-finding-scope-kind f)
                     (binding-finding-path f)
                     (binding-finding-outer-path f))
             (format-finding-recommendation stream f))
    (format stream "~%")))

(defun format-binding-report (findings)
  "Format a list of BINDING-FINDING instances into a human-readable diagnostic report."
  (if (null findings)
      "No unused or shadowed variable bindings detected."
      (let ((unused (remove-if-not (lambda (f) (eq (binding-finding-kind f) :unused-variable)) findings))
            (shadowed (remove-if-not (lambda (f) (eq (binding-finding-kind f) :shadowed-variable)) findings)))
        (with-output-to-string (s)
          (format s "Variable Scope & Binding Report (~A finding~:P):~%~%" (length findings))
          (format-unused-bindings s unused)
          (format-shadowed-bindings s shadowed)))))

(defstruct refactoring-suggestion
  category
  priority
  path
  description
  recommended-tool
  action-plan)

(defun priority-rank (p)
  "Return numeric rank for PRIORITY keyword (:high = 3, :medium = 2, :low = 1)."
  (case p
    (:high 3)
    (:medium 2)
    (:low 1)
    (t 0)))

(defun parse-priority-keyword (val)
  "Parse a priority value into :high, :medium, or :low keyword."
  (cond
    ((null val) :low)
    ((symbolp val) (intern (string-upcase (symbol-name val)) :keyword))
    ((stringp val) (intern (string-upcase (string-left-trim ":" val)) :keyword))
    (t :low)))

(defun parse-category-keyword (val)
  "Parse a category string or keyword into :lint, :complexity, :duplicate, or :binding."
  (let ((s (if (symbolp val) (symbol-name val) (string val))))
    (intern (string-upcase (string-left-trim ":" s)) :keyword)))

(defun category-active-p (cat-keywords primary-keyword &optional alias)
  "Check if PRIMARY-KEYWORD or ALIAS is selected in CAT-KEYWORDS list (or if list is empty)."
  (or (null cat-keywords)
      (member primary-keyword cat-keywords)
      (and alias (member alias cat-keywords))))

(defun collect-lint-suggestions (tree path dialect min-rank)
  "Collect refactoring suggestions generated from the anti-pattern linter."
  (let ((suggestions '())
        (lint-findings (lint-ast tree :path path :dialect dialect)))
    (dolist (f lint-findings)
      (let* ((rule (lint-finding-rule f))
             (p (case rule
                  ((:single-clause-cond :if-boolean-redundant) :high)
                  ((:if-progn-to-when :if-nil-to-when :if-not-to-unless :invert-if-not :equal-nil-to-null) :medium)
                  (t :low)))
             (tool (case rule
                     (:redundant-progn "ast_remove")
                     (t "ast_modify")))
             (plan (if (lint-finding-suggested-fix f)
                       (format nil "Apply structural replacement: ~A" (lint-finding-suggested-fix f))
                       "Refactor expression using recommended pattern.")))
        (when (>= (priority-rank p) min-rank)
          (push (make-refactoring-suggestion
                 :category :lint
                 :priority p
                 :path (lint-finding-path f)
                 :description (lint-finding-message f)
                 :recommended-tool tool
                 :action-plan plan)
                suggestions))))
    (nreverse suggestions)))

(defun complexity-metric->suggestion (m min-rank)
  "Convert a complexity metric M to a refactoring suggestion if it meets MIN-RANK."
  (let* ((cc (complexity-metrics-cyclomatic-complexity m))
         (depth (complexity-metrics-max-nesting-depth m))
         (p (cond
              ((or (>= cc 15) (>= depth 8)) :high)
              ((or (>= cc 10) (>= depth 6)) :medium)
              (t :low))))
    (when (>= (priority-rank p) min-rank)
      (let ((tool (if (>= cc 10) "ast_extract_function" "ast_extract_variable"))
            (plan (format nil "Decompose ~A ~A: ~{~A~^ ~}"
                          (complexity-metrics-kind m)
                          (complexity-metrics-name m)
                          (complexity-metrics-recommendations m))))
        (make-refactoring-suggestion
         :category :complexity
         :priority p
         :path (complexity-metrics-path m)
         :description (format nil "Form has cyclomatic complexity ~A and nesting depth ~A." cc depth)
         :recommended-tool tool
         :action-plan plan)))))

(defun collect-complexity-suggestions (tree path dialect min-rank)
  "Collect refactoring suggestions generated from complexity metrics analysis."
  (let ((complex-forms (analyze-complexity tree :path path :dialect dialect :min-complexity 8 :min-depth 5))
        (suggestions '()))
    (dolist (m complex-forms (nreverse suggestions))
      (let ((s (complexity-metric->suggestion m min-rank)))
        (when s (push s suggestions))))))

(defun duplicate-group->suggestion (g min-rank)
  "Convert a duplicate subtree group G to a refactoring suggestion if it meets MIN-RANK."
  (let* ((savings (* (duplicate-group-node-count g) (1- (duplicate-group-occurrence-count g))))
         (p (cond
              ((>= savings 20) :high)
              ((>= savings 8) :medium)
              (t :low))))
    (when (>= (priority-rank p) min-rank)
      (let ((tool (if (search "ast_extract_variable" (duplicate-group-recommendation g))
                      "ast_extract_variable"
                      "ast_extract_function"))
            (plan (format nil "~A (Saves ~A AST nodes across ~A occurrences)"
                          (duplicate-group-recommendation g)
                          savings
                          (duplicate-group-occurrence-count g))))
        (make-refactoring-suggestion
         :category :duplicate
         :priority p
         :path (first (duplicate-group-paths g))
         :description (format nil "Code clone repeated ~A times: ~A"
                              (duplicate-group-occurrence-count g)
                              (duplicate-group-code-snippet g))
         :recommended-tool tool
         :action-plan plan)))))

(defun collect-duplicate-suggestions (tree path min-rank)
  "Collect refactoring suggestions generated from AST duplicate subtree detection."
  (let ((duplicate-groups (find-duplicate-subtrees tree :path path :min-nodes 5 :min-depth 2))
        (suggestions '()))
    (dolist (g duplicate-groups (nreverse suggestions))
      (let ((s (duplicate-group->suggestion g min-rank)))
        (when s (push s suggestions))))))

(defun binding-finding->suggestion (f min-rank)
  "Convert a binding finding F to a refactoring suggestion if it meets MIN-RANK."
  (let* ((is-shadowed (eq (binding-finding-kind f) :shadowed-variable))
         (p (if is-shadowed :high :medium)))
    (when (>= (priority-rank p) min-rank)
      (let ((tool (if is-shadowed "ast_rename" "ast_remove"))
            (plan (binding-finding-recommendation f)))
        (make-refactoring-suggestion
         :category :binding
         :priority p
         :path (binding-finding-path f)
         :description (binding-finding-message f)
         :recommended-tool tool
         :action-plan plan)))))

(defun collect-binding-suggestions (tree path dialect min-rank)
  "Collect refactoring suggestions generated from lexical scope & binding analysis."
  (let ((binding-findings (analyze-bindings tree :path path :dialect dialect))
        (suggestions '()))
    (dolist (f binding-findings (nreverse suggestions))
      (let ((s (binding-finding->suggestion f min-rank)))
        (when s (push s suggestions))))))

(defun sort-refactoring-suggestions (suggestions)
  "Sort SUGGESTIONS descending by priority rank, breaking ties alphabetically by category."
  (sort suggestions
        (lambda (a b)
          (let ((r-a (priority-rank (refactoring-suggestion-priority a)))
                (r-b (priority-rank (refactoring-suggestion-priority b))))
            (if (= r-a r-b)
                (string< (string (refactoring-suggestion-category a))
                         (string (refactoring-suggestion-category b)))
                (> r-a r-b))))))

(defun suggest-refactorings (tree &key path (min-priority :low) categories (dialect *current-dialect*))
  "Aggregate findings from linting, complexity metrics, clone detection, and binding analysis.
Filters by MIN-PRIORITY (:high, :medium, :low) and CATEGORIES (list of category keywords or strings).
Returns a list of REFACTORING-SUGGESTION instances sorted by priority."
  (let* ((min-rank (priority-rank (parse-priority-keyword min-priority)))
         (cat-keywords (when categories
                         (mapcar #'parse-category-keyword categories)))
         (suggestions '()))
    (when (category-active-p cat-keywords :lint)
      (setf suggestions (nconc (collect-lint-suggestions tree path dialect min-rank) suggestions)))
    (when (category-active-p cat-keywords :complexity)
      (setf suggestions (nconc (collect-complexity-suggestions tree path dialect min-rank) suggestions)))
    (when (category-active-p cat-keywords :duplicate :duplicates)
      (setf suggestions (nconc (collect-duplicate-suggestions tree path min-rank) suggestions)))
    (when (category-active-p cat-keywords :binding :bindings)
      (setf suggestions (nconc (collect-binding-suggestions tree path dialect min-rank) suggestions)))
    (sort-refactoring-suggestions suggestions)))

(defun format-refactoring-suggestions (suggestions)
  "Format a list of REFACTORING-SUGGESTION instances into an executive refactoring report."
  (if (null suggestions)
      "No refactoring opportunities detected matching criteria."
      (let* ((total (length suggestions))
             (high-count (count :high suggestions :key #'refactoring-suggestion-priority))
             (med-count (count :medium suggestions :key #'refactoring-suggestion-priority))
             (low-count (count :low suggestions :key #'refactoring-suggestion-priority))
             (lint-count (count :lint suggestions :key #'refactoring-suggestion-category))
             (comp-count (count :complexity suggestions :key #'refactoring-suggestion-category))
             (dup-count (count :duplicate suggestions :key #'refactoring-suggestion-category))
             (bind-count (count :binding suggestions :key #'refactoring-suggestion-category)))
        (with-output-to-string (s)
          (format s "========================================================~%")
          (format s "    Structural Refactoring Plan (~A Opportunit~:@P)     ~%" total)
          (format s "========================================================~%")
          (format s "Summary by Priority:~%")
          (format s "  - HIGH:   ~A~%" high-count)
          (format s "  - MEDIUM: ~A~%" med-count)
          (format s "  - LOW:    ~A~%~%" low-count)
          (format s "Summary by Category:~%")
          (format s "  - Anti-pattern Linting: ~A~%" lint-count)
          (format s "  - Structural Complexity: ~A~%" comp-count)
          (format s "  - Duplicate Code Clones: ~A~%" dup-count)
          (format s "  - Variable Bindings:     ~A~%~%" bind-count)
          (format s "--------------------------------------------------------~%")
          (format s "Prioritized Action Items:~%~%")
          (loop for item in suggestions
                for idx from 1
                for p = (refactoring-suggestion-priority item)
                for cat = (refactoring-suggestion-category item)
                for path = (refactoring-suggestion-path item)
                for desc = (refactoring-suggestion-description item)
                for tool = (refactoring-suggestion-recommended-tool item)
                for plan = (refactoring-suggestion-action-plan item)
                do
                (format s "~A. [~A | ~A] [~{~A~^, ~}]~%   Problem: ~A~%   Tool:    ~A~%   Action:  ~A~%~%"
                        idx (string p) (string cat) path desc tool plan))))))


