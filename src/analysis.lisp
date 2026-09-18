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

(defun check-single-clause-cond (node path dialect)
  "Detect (cond (<test> <body...>)) with a single clause."
  (declare (ignore dialect))
  (let ((children (get-node-children node)))
    (when (and (compound-node-p node)
               (= (length children) 2)
               (leaf-symbol-p (first children) "COND"))
      (let* ((clause (second children))
             (clause-children (get-node-children clause)))
        (when (and (compound-node-p clause)
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

(defun check-nested-let (node path dialect)
  "Detect nested (let ((x ...)) (let ((y ...)) ...)) that could be combined into let*."
  (declare (ignore dialect))
  (let ((children (get-node-children node)))
    (when (and (compound-node-p node)
               (>= (length children) 3)
               (leaf-symbol-p (first children) "LET"))
      (let* ((outer-bindings (second children))
             (outer-body (nthcdr 2 children)))
        (when (and (= (length outer-body) 1)
                   (compound-node-p outer-bindings))
          (let* ((inner-node (first outer-body))
                 (inner-children (get-node-children inner-node)))
            (when (and (compound-node-p inner-node)
                       (>= (length inner-children) 3)
                       (leaf-symbol-p (first inner-children) "LET"))
              (let* ((inner-bindings (second inner-children))
                     (inner-body (nthcdr 2 inner-children)))
                (when (compound-node-p inner-bindings)
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
      (when (and (compound-node-p node)
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

(defstruct (complexity-metrics (:constructor make-complexity-metrics))
  name
  kind
  path
  cyclomatic-complexity
  max-nesting-depth
  form-count
  recommendations)

(defun form-definition-info (node)
  "If NODE is a definition (defun, defmacro, defmethod, defgeneric, defn, define),
return (values is-def-p name-str kind-keyword)."
  (let ((children (get-node-children node)))
    (when (and (compound-node-p node)
               (>= (length children) 2))
      (let ((head (first children)))
        (multiple-value-bind (path tag val) (parse-node head)
          (declare (ignore path tag))
          (when (symbolp val)
            (let* ((head-name (string-upcase (symbol-name val)))
                   (name-node (second children))
                   (kind (cond
                           ((member head-name '("DEFUN" "DEFN" "DEFN-") :test #'string=) :function)
                           ((member head-name '("DEFMACRO" "DEFSYNTAX") :test #'string=) :macro)
                           ((string= head-name "DEFMETHOD") :method)
                           ((string= head-name "DEFGENERIC") :generic))))
              (cond
                (kind
                 (values t (format-atom (nth-value 2 (parse-node name-node))) kind))
                ((string= head-name "DEFINE")
                 (if (compound-node-p name-node)
                     (let ((fn-head (first (get-node-children name-node))))
                       (values t (format-atom (nth-value 2 (parse-node fn-head))) :function))
                     (values t (format-atom (nth-value 2 (parse-node name-node))) :definition)))
                (t (values nil nil nil))))))))))

(defun compute-branch-complexity (node &optional (dialect :common-lisp))
  "Compute McCabe cyclomatic complexity of NODE.
Base complexity is 1, with +1 for each conditional branch, short-circuit point, loop, or handler."
  (let ((complexity 1))
    (labels ((walk (curr)
               (let ((children (get-node-children curr)))
                 (when (and (compound-node-p curr)
                            children)
                   (let ((head (first children)))
                     (multiple-value-bind (p t-val val) (parse-node head)
                       (declare (ignore p t-val))
                       (when (symbolp val)
                         (let ((name (string-upcase (symbol-name val))))
                           (cond
                             ((member name '("IF" "WHEN" "UNLESS" "WHEN-NOT" "IF-NOT"
                                             "WHEN-LET" "IF-LET" "WHEN-FIRST")
                                      :test #'string=)
                              (incf complexity))
                             ((string= name "COND")
                              (dolist (clause (rest children))
                                (let ((c-children (get-node-children clause)))
                                  (when (and (compound-node-p clause)
                                             c-children)
                                    (let ((test (first c-children)))
                                      (unless (or (leaf-true-p test dialect)
                                                  (leaf-symbol-p test "OTHERWISE")
                                                  (leaf-symbol-p test ":ELSE"))
                                        (incf complexity)))))))
                             ((member name '("CASE" "CCASE" "ECASE" "TYPECASE" "CTYPECASE"
                                             "ETYPECASE" "CONDP")
                                      :test #'string=)
                              (dolist (clause (nthcdr 2 children))
                                (let ((c-children (get-node-children clause)))
                                  (when (and (compound-node-p clause)
                                             c-children)
                                    (let ((selector (first c-children)))
                                      (unless (or (leaf-true-p selector dialect)
                                                  (leaf-symbol-p selector "OTHERWISE"))
                                        (incf complexity)))))))
                             ((member name '("AND" "OR") :test #'string=)
                              (when (> (length children) 2)
                                (incf complexity (- (length children) 2))))
                             ((member name '("LOOP" "DOLIST" "DOTIMES" "DO" "DO*" "DOSEQ" "RECUR")
                                      :test #'string=)
                              (incf complexity))
                             ((member name '("HANDLER-CASE" "RESTART-CASE") :test #'string=)
                              (dolist (clause (nthcdr 2 children))
                                (when (compound-node-p clause)
                                  (incf complexity))))))))))
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
    (labels ((collect-file (file-node f-path)
               (loop for form in (get-node-children file-node)
                     for form-idx from 0
                     for form-path = (or (get-node-path form) (append f-path (list form-idx)))
                     do (push (cons form form-path) results)))
             (collect-dialect (dialect-node d-path)
               (loop for file-child in (get-node-children dialect-node)
                     for f-idx from 0
                     for f-path = (or (get-node-path file-child) (append d-path (list f-idx)))
                     do (collect-file file-child f-path))))
      (cond
        ((eq tag :workspace)
         (loop for dialect-child in (get-node-children node)
               for d-idx from 0
               for d-path = (or (get-node-path dialect-child) (append base-path (list d-idx)))
               do (collect-dialect dialect-child d-path)))
        ((supported-dialect-p tag)
         (collect-dialect node base-path))
        ((eq tag :file)
         (collect-file node base-path))
        (t
         (push (cons node (or (get-node-path node) base-path)) results)))
      (nreverse results))))

(defun analyze-complexity (tree &key path dialect (min-complexity 1) (min-depth 1))
  "Analyze structural complexity for forms in TREE (or under PATH).
Filters results to those meeting MIN-COMPLEXITY and MIN-DEPTH thresholds."
  (let* ((start-node (resolve-tree-scope tree path))
         (forms-with-paths (when start-node
                               (collect-top-level-forms start-node path)))
         (results '()))
    (dolist (pair forms-with-paths)
      (let* ((form-node (car pair))
             (form-path (cdr pair))
             (metrics (analyze-form-complexity form-node :path form-path :dialect dialect)))
        (when (and (>= (complexity-metrics-cyclomatic-complexity metrics) (or min-complexity 1))
                   (>= (complexity-metrics-max-nesting-depth metrics) (or min-depth 1)))
          (push metrics results))))
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

(defun find-duplicate-subtrees (tree &key path (min-nodes 4) (min-depth 2) (exact t))
  "Find repeated AST subtrees in TREE (or under PATH).
Groups matching subtrees, removes redundant subsumed sub-expressions, and generates refactoring recommendations."
  (let* ((start-node (resolve-tree-scope tree path))
          (buckets (make-hash-table :test 'equal))
          (node-metadata (make-hash-table :test 'equal)))
    (when start-node
      (labels ((harvest (curr curr-path &optional context)
                 (let ((tag (get-node-tag curr))
                       (children (get-node-children curr)))
                   (when (and (member tag '(:paren :square :curly))
                              children
                              (not (member tag '(:workspace :common-lisp :clojure :scheme :emacs-lisp :fennel))))
                     (let* ((first-child (first children))
                            (first-val (when (and first-child (eq (get-node-tag first-child) :leaf))
                                         (nth-value 2 (parse-node first-child))))
                            (head-str (when (symbolp first-val) (string-upcase (symbol-name first-val)))))
                       (unless (or (member context '(:binding-list :binding-clause))
                                   (member head-str *compiler-directive-heads* :test #'string=))
                         (let ((node-cnt (count-ast-nodes curr))
                               (depth (compute-nesting-depth curr 0)))
                           (when (and (>= node-cnt (or min-nodes 4))
                                      (>= depth (or min-depth 2)))
                             (let ((fingerprint (canonicalize-subtree curr :exact exact)))
                               (push (cons curr-path curr) (gethash fingerprint buckets nil))
                               (unless (gethash fingerprint node-metadata)
                                 (setf (gethash fingerprint node-metadata)
                                       (list :node-count node-cnt :depth depth :sample curr)))))))))
                   (let* ((first-child (first children))
                          (first-val (when (and first-child (eq (get-node-tag first-child) :leaf))
                                       (nth-value 2 (parse-node first-child))))
                          (head-str (when (symbolp first-val) (string-upcase (symbol-name first-val)))))
                     (unless (member head-str '("DECLARE" "DECLAIM" "PROCLAIM" "IN-PACKAGE" "DEFPACKAGE") :test #'string=)
                       (flet ((resolve-child-path (child idx)
                                (or (get-node-path child) (append curr-path (list idx))))
                              (recurse-children (child-list child-context)
                                (loop for child in child-list
                                      for idx from 0
                                      for cp = (or (get-node-path child) (append curr-path (list idx)))
                                      do (harvest child cp child-context))))
                         (cond
                           ;; Clojure-style vector binding list [k1 v1 k2 v2]
                           ((and (eq context :binding-list) (eq tag :square))
                            (loop for child in children
                                  for idx from 0
                                  for cp = (resolve-child-path child idx)
                                  do (harvest child cp (if (evenp idx) :binding-clause nil))))
                           ;; Lisp-style binding list ((var1 val1) (var2 val2))
                           ((eq context :binding-list)
                            (recurse-children children :binding-clause))
                           ;; Individual binding clause (var val) or (fn (params) body)
                           ((eq context :binding-clause)
                            (loop for child in (rest children)
                                  for idx from 1
                                  for cp = (resolve-child-path child idx)
                                  do (harvest child cp nil)))
                           ;; Forms where child 1 is the bindings list
                           ((or (member head-str *binding-form-heads* :test #'string=)
                                (member head-str '("MULTIPLE-VALUE-BIND" "DESTRUCTURING-BIND") :test #'string=)
                                (and (equal head-str "LOOP") children (second children) (eq (get-node-tag (second children)) :square)))
                            (loop for child in children
                                  for idx from 0
                                  for cp = (resolve-child-path child idx)
                                  do (harvest child cp (if (= idx 1) :binding-list nil))))
                           ;; Default traversal
                           (t
                            (recurse-children children nil)))))))))
        (harvest start-node (or (get-node-path start-node) path '()))))

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

      (let ((filtered-candidates '()))
        (dolist (cand raw-candidates)
          (let* ((c-paths (getf cand :paths))
                 (c-count (length c-paths))
                 (subsumed-p
                   (some (lambda (other)
                           (unless (eq cand other)
                             (let ((o-paths (getf other :paths))
                                   (o-count (length (getf other :paths))))
                               (and (= c-count o-count)
                                    (> (getf other :node-count) (getf cand :node-count))
                                    (every (lambda (cp)
                                             (some (lambda (op) (path-prefix-p op cp)) o-paths))
                                           c-paths)))))
                         raw-candidates)))
            (unless subsumed-p
              (push cand filtered-candidates))))

        (let ((groups
                (mapcar
                 (lambda (cand)
                   (let* ((paths (getf cand :paths))
                          (sample (getf cand :sample))
                          (raw-str (sexp-to-string sample))
                          (single-line (substitute #\Space #\Newline (string-trim '(#\Space #\Newline #\Tab) raw-str)))
                          (snippet (if (> (length single-line) 80)
                                       (format nil "~A..." (subseq single-line 0 77))
                                       single-line))
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
                 filtered-candidates)))
          (sort groups
                (lambda (a b)
                  (let ((savings-a (* (duplicate-group-node-count a) (1- (duplicate-group-occurrence-count a))))
                        (savings-b (* (duplicate-group-node-count b) (1- (duplicate-group-occurrence-count b)))))
                    (if (= savings-a savings-b)
                        (> (duplicate-group-occurrence-count a) (duplicate-group-occurrence-count b))
                        (> savings-a savings-b))))))))))

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
            (when (eq (get-node-tag spec) :paren)
              (let* ((spec-children (get-node-children spec))
                     (spec-name (when spec-children (leaf-symbol-name (first spec-children)))))
                (when (member spec-name '("ignore" "ignorable") :test #'string=)
                  (dolist (var-node (rest spec-children))
                    (when (leaf-any-symbol-p var-node)
                      (push (leaf-symbol-name var-node) ignored)))))))
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

(defun extract-param-bindings (params-node)
  "Extract list of (name . path) pairs from PARAMS-NODE."
  (let ((results '()))
    (labels ((collect (node)
               (when node
                 (let ((tag (get-node-tag node)))
                   (cond
                     ((leaf-any-symbol-p node)
                      (let ((name (leaf-symbol-name node)))
                        (unless (member name *cl-lambda-keywords* :test #'string=)
                          (push (cons name (get-node-path node)) results))))
                     ((member tag '(:paren :square))
                      (let ((children (get-node-children node)))
                        (when children
                          (let ((first-child (first children)))
                            (cond
                              ((and (eq tag :paren)
                                    (or (leaf-any-symbol-p first-child)
                                        (and (eq (get-node-tag first-child) :paren)
                                             (get-node-children first-child))))
                               (if (and (eq (get-node-tag first-child) :paren)
                                        (get-node-children first-child))
                                   (let ((sub (get-node-children first-child)))
                                     (when (>= (length sub) 2)
                                       (collect (second sub))))
                                   (collect first-child))
                               (when (>= (length children) 3)
                                 (collect (third children))))
                              (t
                               (dolist (c children)
                                 (collect c))))))))
                     ((eq tag :curly)
                      (let ((children (get-node-children node)))
                        (loop for (k v) on children by #'cddr
                              while k do
                              (let ((k-name (leaf-symbol-name k)))
                                (cond
                                  ((and (equal k-name ":keys") (member (get-node-tag v) '(:square :paren)))
                                   (dolist (c (get-node-children v))
                                     (collect c)))
                                  ((and (equal k-name ":as") (leaf-any-symbol-p v))
                                   (collect v))
                                  ((leaf-any-symbol-p k)
                                   (collect k))))))))))))
      (if (compound-node-p params-node)
          (dolist (c (get-node-children params-node))
            (collect c))
          (collect params-node)))
    (nreverse results)))

(defun extract-let-clauses (bindings-node dialect)
  "Extract list of plist (:pattern node :init node) from BINDINGS-NODE according to DIALECT."
  (let ((results '()))
    (when bindings-node
      (let ((tag (get-node-tag bindings-node))
            (children (get-node-children bindings-node)))
        (cond
          ((or (eq dialect :clojure) (eq dialect :fennel) (eq tag :square))
           (loop for (pat-node init-node) on children by #'cddr
                 while pat-node do
                 (push (list :pattern pat-node :init init-node) results)))
          (t
           (dolist (clause children)
             (cond
               ((leaf-any-symbol-p clause)
                (push (list :pattern clause :init nil) results))
               ((compound-node-p clause)
                (let ((c-children (get-node-children clause)))
                  (push (list :pattern (first c-children)
                              :init (second c-children))
                        results)))))))))
    (nreverse results)))

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

(defun walk-binding-tree (node scope dialect findings-acc)
  "Recursively walk NODE in SCOPE, updating findings accumulator and resolving variable usages."
  (when node
    (flet ((walk-all (node-list target-scope)
             (dolist (item node-list)
               (setf findings-acc (walk-binding-tree item target-scope dialect findings-acc)))))
      (let ((tag (get-node-tag node))
            (children (get-node-children node)))
        (cond
          ((eq tag :leaf)
           (when (leaf-any-symbol-p node)
             (record-variable-usage scope (leaf-symbol-name node) (get-node-path node))))

          ((eq tag :comment)
           nil)

          ((or (eq tag :workspace) (eq tag :file) (supported-dialect-p tag) (member tag '(:curly :set)))
           (walk-all children scope))

          ((member tag '(:paren :square))
           (when children
             (let* ((head (first children))
                    (head-name (when (leaf-any-symbol-p head) (leaf-symbol-name head))))
               (cond
                 ((member head-name '("quote" "'") :test #'string=)
                  nil)

                 ((and (not (lisp-1-dialect-p dialect))
                       (member head-name '("function" "#'") :test #'string=))
                  nil)

                 ((equal head-name "declare")
                  nil)

                 ((member head-name '("defun" "defmacro" "defmethod" "defn" "defn-" "define") :test #'string=)
                  (let* ((name-child (second children))
                         (fn-name (if (leaf-any-symbol-p name-child) (leaf-symbol-name name-child) "anonymous"))
                         (params-node nil)
                         (body-nodes nil))
                    (cond
                      ((and (equal head-name "define") (compound-node-p name-child))
                       (let ((sig-children (get-node-children name-child)))
                         (when sig-children
                           (setf fn-name (leaf-symbol-name (first sig-children)))
                           (setf params-node (list* :path (get-node-path name-child) :paren (rest sig-children)))
                           (setf body-nodes (cddr children)))))

                      ((or (eq dialect :clojure) (member head-name '("defn" "defn-") :test #'string=))
                       (let ((rem (cddr children)))
                         (when (and rem (or (stringp (third (parse-node (first rem))))
                                            (eq (get-node-tag (first rem)) :curly)))
                           (setf rem (rest rem)))
                         (if (and rem (eq (get-node-tag (first rem)) :square))
                             (progn
                               (setf params-node (first rem))
                               (setf body-nodes (rest rem)))
                             (setf body-nodes rem))))

                      (t
                       (setf params-node (third children))
                       (setf body-nodes (cdddr children))))

                    (if params-node
                        (multiple-value-bind (ignored rem-body)
                            (get-scope-declarations body-nodes dialect)
                          (let* ((fn-scope (make-lexical-scope :kind :function
                                                               :parent scope
                                                               :dialect dialect
                                                               :enclosing-name fn-name))
                                 (params (extract-param-bindings params-node)))
                            (dolist (p params)
                              (setf findings-acc (check-and-register-binding fn-scope (car p) (cdr p) ignored findings-acc)))
                            (walk-all rem-body fn-scope)
                            (setf findings-acc (check-unused-in-scope fn-scope dialect findings-acc))))
                        (dolist (form body-nodes)
                          (if (compound-node-p form)
                              (let* ((f-children (get-node-children form))
                                     (p-node (first f-children))
                                     (b-nodes (rest f-children)))
                                (if (and p-node (compound-node-p p-node))
                                    (let* ((fn-scope (make-lexical-scope :kind :function
                                                                         :parent scope
                                                                         :dialect dialect
                                                                         :enclosing-name fn-name))
                                           (params (extract-param-bindings p-node)))
                                      (dolist (p params)
                                        (setf findings-acc (check-and-register-binding fn-scope (car p) (cdr p) nil findings-acc)))
                                      (walk-all b-nodes fn-scope)
                                      (setf findings-acc (check-unused-in-scope fn-scope dialect findings-acc)))
                                    (setf findings-acc (walk-binding-tree form scope dialect findings-acc))))
                              (setf findings-acc (walk-binding-tree form scope dialect findings-acc)))))))

                 ((member head-name '("lambda" "fn") :test #'string=)
                  (let* ((rem (rest children))
                         (params-node nil)
                         (body-nodes nil))
                    (when (and rem (leaf-any-symbol-p (first rem)) (rest rem))
                      (setf rem (rest rem)))
                    (when rem
                      (setf params-node (first rem))
                      (setf body-nodes (rest rem)))
                    (if (and params-node (compound-node-p params-node))
                        (multiple-value-bind (ignored rem-body)
                            (get-scope-declarations body-nodes dialect)
                          (let* ((lam-scope (make-lexical-scope :kind :lambda
                                                                :parent scope
                                                                :dialect dialect
                                                                :enclosing-name "lambda"))
                                 (params (extract-param-bindings params-node)))
                            (dolist (p params)
                              (setf findings-acc (check-and-register-binding lam-scope (car p) (cdr p) ignored findings-acc)))
                            (walk-all rem-body lam-scope)
                            (setf findings-acc (check-unused-in-scope lam-scope dialect findings-acc))))
                        (walk-all (rest children) scope))))

                 ((and (equal head-name "let") (not (vector-binding-dialect-p dialect)))
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
                        (walk-all rem-body let-scope)
                        (setf findings-acc (check-unused-in-scope let-scope dialect findings-acc))))))

                 ((or (member head-name '("let*" "letrec") :test #'string=)
                      (and (member head-name '("let" "loop") :test #'string=)
                           (vector-binding-dialect-p dialect)))
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
                      (walk-all rem-body curr-scope)
                      (dolist (sc created-scopes)
                        (setf findings-acc (check-unused-in-scope sc dialect findings-acc))))))

                 ((equal head-name "loop")
                  (walk-all (rest children) scope))

                 ((equal head-name "multiple-value-bind")
                  (let* ((vars-node (second children))
                         (val-node (third children))
                         (body-nodes (cdddr children)))
                    (setf findings-acc (walk-binding-tree val-node scope dialect findings-acc))
                    (multiple-value-bind (ignored rem-body)
                        (get-scope-declarations body-nodes dialect)
                      (let ((mvb-scope (make-lexical-scope :kind :multiple-value-bind :parent scope :dialect dialect))
                            (bound (extract-param-bindings vars-node)))
                        (dolist (p bound)
                          (setf findings-acc (check-and-register-binding mvb-scope (car p) (cdr p) ignored findings-acc)))
                        (walk-all rem-body mvb-scope)
                        (setf findings-acc (check-unused-in-scope mvb-scope dialect findings-acc))))))

                 ((equal head-name "destructuring-bind")
                  (let* ((pat-node (second children))
                         (expr-node (third children))
                         (body-nodes (cdddr children)))
                    (setf findings-acc (walk-binding-tree expr-node scope dialect findings-acc))
                    (multiple-value-bind (ignored rem-body)
                        (get-scope-declarations body-nodes dialect)
                      (let ((db-scope (make-lexical-scope :kind :destructuring-bind :parent scope :dialect dialect))
                            (bound (extract-param-bindings pat-node)))
                        (dolist (p bound)
                          (setf findings-acc (check-and-register-binding db-scope (car p) (cdr p) ignored findings-acc)))
                        (walk-all rem-body db-scope)
                        (setf findings-acc (check-unused-in-scope db-scope dialect findings-acc))))))

                 ((member head-name '("dolist" "dotimes") :test #'string=)
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
                              (walk-all rem-body loop-scope)
                              (when res-form
                                (setf findings-acc (walk-binding-tree res-form loop-scope dialect findings-acc)))
                              (setf findings-acc (check-unused-in-scope loop-scope dialect findings-acc)))))
                        (walk-all (rest children) scope))))

                 ((member head-name '("when-let" "if-let" "when-some" "if-some") :test #'string=)
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
                    (walk-all body-nodes wl-scope)
                    (setf findings-acc (check-unused-in-scope wl-scope dialect findings-acc))))

                 ((member head-name '("flet" "labels") :test #'string=)
                  (let* ((fns-node (second children))
                         (body-nodes (cddr children)))
                    (when (and fns-node (compound-node-p fns-node))
                      (dolist (f-def (get-node-children fns-node))
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
                                  (walk-all rem-body loc-scope)
                                  (setf findings-acc (check-unused-in-scope loc-scope dialect findings-acc)))))))))
                    (walk-all body-nodes scope)))

                 (t
                  (when (and (lisp-1-dialect-p dialect)
                             (leaf-any-symbol-p head)
                             (not (member head-name *lisp-special-operators* :test #'string=)))
                    (record-variable-usage scope head-name (get-node-path head)))
                  (walk-all (rest children) scope))))))))))
    findings-acc)

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

(defun suggest-refactorings (tree &key path (min-priority :low) categories (dialect *current-dialect*))
  "Aggregate findings from linting, complexity metrics, clone detection, and binding analysis.
Filters by MIN-PRIORITY (:high, :medium, :low) and CATEGORIES (list of category keywords or strings).
Returns a list of REFACTORING-SUGGESTION instances sorted by priority."
  (let* ((min-rank (priority-rank (parse-priority-keyword min-priority)))
         (cat-keywords (when categories
                         (mapcar #'parse-category-keyword categories)))
         (suggestions '()))

    (when (or (null cat-keywords) (member :lint cat-keywords))
      (let ((lint-findings (lint-ast tree :path path :dialect dialect)))
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
                    suggestions))))))

    (when (or (null cat-keywords) (member :complexity cat-keywords))
      (let ((complex-forms (analyze-complexity tree :path path :dialect dialect :min-complexity 8 :min-depth 5)))
        (dolist (m complex-forms)
          (let* ((cc (complexity-metrics-cyclomatic-complexity m))
                 (depth (complexity-metrics-max-nesting-depth m))
                 (p (if (or (>= cc 15) (>= depth 8))
                        :high
                        (if (or (>= cc 10) (>= depth 6))
                            :medium
                            :low)))
                 (tool (if (>= cc 10) "ast_extract_function" "ast_extract_variable"))
                 (plan (format nil "Decompose ~A ~A: ~{~A~^ ~}"
                               (complexity-metrics-kind m)
                               (complexity-metrics-name m)
                               (complexity-metrics-recommendations m))))
            (when (>= (priority-rank p) min-rank)
              (push (make-refactoring-suggestion
                     :category :complexity
                     :priority p
                     :path (complexity-metrics-path m)
                     :description (format nil "Form has cyclomatic complexity ~A and nesting depth ~A." cc depth)
                     :recommended-tool tool
                     :action-plan plan)
                    suggestions))))))

    (when (or (null cat-keywords) (member :duplicate cat-keywords) (member :duplicates cat-keywords))
      (let ((duplicate-groups (find-duplicate-subtrees tree :path path :min-nodes 5 :min-depth 2)))
        (dolist (g duplicate-groups)
          (let* ((savings (* (duplicate-group-node-count g) (1- (duplicate-group-occurrence-count g))))
                 (p (cond
                      ((>= savings 20) :high)
                      ((>= savings 8) :medium)
                      (t :low)))
                 (tool (if (search "ast_extract_variable" (duplicate-group-recommendation g))
                           "ast_extract_variable"
                           "ast_extract_function"))
                 (plan (format nil "~A (Saves ~A AST nodes across ~A occurrences)"
                               (duplicate-group-recommendation g)
                               savings
                               (duplicate-group-occurrence-count g))))
            (when (>= (priority-rank p) min-rank)
              (push (make-refactoring-suggestion
                     :category :duplicate
                     :priority p
                     :path (first (duplicate-group-paths g))
                     :description (format nil "Code clone repeated ~A times: ~A"
                                          (duplicate-group-occurrence-count g)
                                          (duplicate-group-code-snippet g))
                     :recommended-tool tool
                     :action-plan plan)
                    suggestions))))))

    (when (or (null cat-keywords) (member :binding cat-keywords) (member :bindings cat-keywords))
      (let ((binding-findings (analyze-bindings tree :path path :dialect dialect)))
        (dolist (f binding-findings)
          (let* ((is-shadowed (eq (binding-finding-kind f) :shadowed-variable))
                 (p (if is-shadowed :high :medium))
                 (tool (if is-shadowed "ast_rename" "ast_remove"))
                 (plan (binding-finding-recommendation f)))
            (when (>= (priority-rank p) min-rank)
              (push (make-refactoring-suggestion
                     :category :binding
                     :priority p
                     :path (binding-finding-path f)
                     :description (binding-finding-message f)
                     :recommended-tool tool
                     :action-plan plan)
                    suggestions))))))

    (sort suggestions
          (lambda (a b)
            (let ((r-a (priority-rank (refactoring-suggestion-priority a)))
                  (r-b (priority-rank (refactoring-suggestion-priority b))))
              (if (= r-a r-b)
                  (string< (string (refactoring-suggestion-category a))
                           (string (refactoring-suggestion-category b)))
                  (> r-a r-b)))))))

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


