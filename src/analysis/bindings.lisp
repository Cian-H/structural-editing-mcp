(in-package :structural-editing-mcp.analysis)

(declaim (optimize (speed 2) (safety 3)))

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
  "Extract ignored/ignorable variable names from leading declarations and docstrings in BODY-NODES.
Returns (values ignored-names remaining-body-nodes)."
  (let ((ignored '())
        (remaining body-nodes)
        (seen-docstring nil))
    (loop while remaining
          for form = (first remaining)
          for tag = (get-node-tag form)
          for children = (get-node-children form)
          do (cond
               ((and (not seen-docstring)
                     (rest remaining)
                     (eq tag :leaf)
                     (stringp (third (parse-node form))))
                 (setf seen-docstring t)
                 (setf remaining (rest remaining)))
               ((and (eq tag :paren)
                     children
                     (equal (leaf-symbol-name (first children)) "declare"))
                 (dolist (spec (rest children))
                   (dolist (var (extract-spec-ignored-vars spec))
                     (push var ignored)))
                 (setf remaining (rest remaining)))
               (t
                 (return))))
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
              :message (fmt "Variable '~A' in ~A shadows outer binding at [~{~A~^, ~}]."
                            name (lexical-scope-kind scope) (scope-binding-path outer))
              :recommendation (fmt "Consider renaming local variable '~A' using 'ast_rename' to avoid shadowing." name))
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
                   (fmt "If intentionally unused, prefix with '_' (e.g. '_~A')." name)
                   (fmt "If intentionally unused, prefix with '_' or add '(declare (ignore ~A))'." name))
                 (fmt "Variable '~A' is unused. Consider removing it with 'ast_remove' or prefixing with '_'." name))))
        (push (make-binding-finding
                :kind :unused-variable
                :variable-name name
                :path path
                :scope-kind s-kind
                :outer-path nil
                :message (fmt "Variable '~A' defined in ~A is never used." name s-kind)
                :recommendation recomm)
              findings))))
  findings)

(defun walk-binding-nodes (nodes scope dialect findings-acc)
  "Walk a list of NODES in SCOPE, accumulating binding findings."
  (dolist (item nodes findings-acc)
    (setf findings-acc (walk-binding-tree item scope dialect findings-acc))))

(defun walk-binding-rest-children (children scope dialect findings-acc)
  "Walk (rest CHILDREN) in SCOPE, accumulating binding findings."
  (walk-binding-nodes (rest children) scope dialect findings-acc))

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
      (walk-binding-rest-children children scope dialect findings-acc))))

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
      (walk-binding-rest-children children scope dialect findings-acc))))

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
  (walk-binding-rest-children children scope dialect findings-acc))

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

(defun walk-cond-binding-form (children scope dialect findings-acc)
  "Walk COND clauses, evaluating both test and body expressions in each clause."
  (if (eq dialect :clojure)
    (walk-binding-rest-children children scope dialect findings-acc)
    (dolist (clause (rest children) findings-acc)
      (if (compound-node-p clause)
        (dolist (expr (get-node-children clause))
          (setf findings-acc (walk-binding-tree expr scope dialect findings-acc)))
        (setf findings-acc (walk-binding-tree clause scope dialect findings-acc))))))

(defun walk-case-binding-form (children scope dialect findings-acc)
  "Walk CASE form: keyform is evaluated, each clause has literals then body expressions."
  (when (second children)
    (setf findings-acc (walk-binding-tree (second children) scope dialect findings-acc)))
  (dolist (clause (cddr children) findings-acc)
    (when (compound-node-p clause)
      (dolist (body-form (rest (get-node-children clause)))
        (setf findings-acc (walk-binding-tree body-form scope dialect findings-acc))))))

(defun walk-scope-binding-form
       (head-name children scope dialect findings-acc)
  "Walk lexical bindings, iteration, or fallback forms."
  (match head-name
         ((guard _ (and (equal head-name "let") (not (vector-binding-dialect-p dialect))))
          (walk-cl-let-form children scope dialect findings-acc))
         ((guard _ (sequential-binding-form-p head-name dialect))
          (walk-sequential-let-form head-name children scope dialect findings-acc))
         ("loop" (walk-binding-rest-children children scope dialect findings-acc))
         ("cond" (walk-cond-binding-form children scope dialect findings-acc))
         ((or "case" "ccase" "ecase" "typecase" "ctypecase" "etypecase")
          (walk-case-binding-form children scope dialect findings-acc))
         ((or "multiple-value-bind" "destructuring-bind")
          (walk-bind-form head-name children scope dialect findings-acc))
         ((or "dolist" "dotimes")
          (walk-iteration-binding-form head-name children scope dialect findings-acc))
         ((or "when-let" "if-let" "when-some" "if-some")
          (walk-when-let-form children scope dialect findings-acc))
         ((or "flet" "labels")
          (walk-flet-labels-form children scope dialect findings-acc))
         (_ (walk-fallback-binding-form head-name children scope dialect findings-acc))))

(defun walk-compound-binding-form
       (head-name children scope dialect findings-acc)
  "Dispatch binding analysis for compound forms by operator head name."
  (match head-name
         ((guard _ (binding-ignore-head-p head-name dialect)) findings-acc)
         ((or "defun" "defmacro" "defmethod" "defn" "defn-" "define" "lambda" "fn")
          (walk-function-definition-form head-name children scope dialect findings-acc))
         (_ (walk-scope-binding-form head-name children scope dialect findings-acc))))

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
