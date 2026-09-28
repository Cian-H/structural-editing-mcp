(in-package :structural-editing-mcp.analysis)

(declaim (optimize (speed 2) (safety 3)))

(defstruct (lint-finding (:constructor make-lint-finding))
  rule
  path
  message
  severity
  suggested-fix)

(defvar *current-lint-tree* nil
  "Dynamically bound to the root AST during linting to allow contextual queries (e.g., parent lookup).")

(defun lint-parent-node (path)
  "Return the parent node of PATH using *CURRENT-LINT-TREE*."
  (when (and *current-lint-tree* (consp path))
    (get-node-at-path *current-lint-tree* (butlast path))))

(defun form-in-statement-position-p (node path)
  "Return T if NODE at PATH is in statement position where its return value is discarded.
Checks if the parent is a sequence (progn, let, defun, etc.) and NODE is not the final expression."
  (declare (ignore node))
  (when (and *current-lint-tree* (consp path))
    (let* ((parent (lint-parent-node path))
           (child-idx (first (last path))))
      (when (and parent (compound-node-p parent))
        (let* ((children (get-node-children parent))
               (num-children (length children))
               (head (first children))
               (head-name (when (and head (leaf-any-symbol-p head))
                            (leaf-symbol-name head))))
          (cond
            ;; In (progn e1 e2 ... en), e_i for i < n-1 is in statement position
            ((equal head-name "progn")
              (< child-idx (1- num-children)))
            ;; In (defun / defmacro / defmethod / defn name (...) e1 ... en)
            ((member head-name '("defun" "defmacro" "defmethod" "defn" "defn-") :test #'string=)
              (let ((body-start (if (member head-name '("defn" "defn-") :test #'string=)
                                  (if (and (>= num-children 3) (eq (get-node-tag (third children)) :square)) 3 2)
                                  3)))
                (and (>= child-idx body-start)
                     (< child-idx (1- num-children)))))
            ;; In (let / let* (...) e1 ... en)
            ((member head-name '("let" "let*") :test #'string=)
              (and (>= child-idx 2)
                   (< child-idx (1- num-children))))
            ;; In (do ...)
            ((equal head-name "do")
              (< child-idx (1- num-children)))
            (t nil)))))))

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
  (declare (ignore dialect))
  (let ((children (get-node-children node)))
    (when (and (if-form-p node)
               (= (length children) 4)
               (leaf-nil-p (fourth children)))
      (let ((cond-node (second children))
            (then-node (third children)))
        (unless (or (leaf-true-p then-node)
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
               (let ((head (first children)))
                 (and (leaf-any-symbol-p head)
                      (member (leaf-symbol-name head) '("equal" "eq" "eql") :test #'string=))))
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

;;; ---------------------------------------------------------------------------
;;; P0 Rules: Safety, Destructive Returns, Macro Hygiene, & TCO
;;; ---------------------------------------------------------------------------

(defparameter *destructive-sequence-functions*
  '("delete" "delete-if" "delete-if-not" "sort" "stable-sort" "nreverse" "nconc" "nreconc")
  "Names of destructive functions whose return values must be captured.")

(defun check-ignored-destructive-return (node path dialect)
  "Detect calls to destructive sequence functions in statement position where the return value is discarded."
  (declare (ignore dialect))
  (when (and (compound-node-p node) (consp path))
    (let ((children (get-node-children node)))
      (when (and children (leaf-any-symbol-p (first children)))
        (let ((fn-name (leaf-symbol-name (first children))))
          (when (member fn-name *destructive-sequence-functions* :test #'string=)
            (when (form-in-statement-position-p node path)
              (make-lint-finding
                :rule :ignored-destructive-return
                :path path
                :message (fmt "Return value of destructive function '~A' is ignored; this can cause list truncation or dropped heads." fn-name)
                :severity :warning
                :suggested-fix nil))))))))

(defun find-unhygienic-macro-bindings (body-nodes)
  "Scan BODY-NODES of a defmacro for literal symbol bindings in backquoted let forms."
  (let ((unhygienic '()))
    (labels ((scan (n in-backquote)
               (cond
                 ((null n) nil)
                 ((eq (get-node-tag n) :leaf)
                   (let ((val (parse-atom-string (format-atom (third (parse-node n))))))
                     (declare (ignore val))))
                 ((compound-node-p n)
                   (let* ((children (get-node-children n))
                          (head (first children))
                          (head-name (when (and head (leaf-any-symbol-p head)) (leaf-symbol-name head))))
                     (cond
                       ((and in-backquote (member head-name '("let" "let*") :test #'string=) (second children))
                         (let ((bindings-node (second children)))
                           (when (compound-node-p bindings-node)
                             (dolist (clause (get-node-children bindings-node))
                               (when (compound-node-p clause)
                                 (let ((var-node (first (get-node-children clause))))
                                   (when (and var-node (leaf-any-symbol-p var-node))
                                     (let ((name (leaf-symbol-name var-node)))
                                       (unless (or (string-prefix-p "," name)
                                                   (string-prefix-p "#" name))
                                         (push (list :name name :path (get-node-path var-node)) unhygienic))))))))))
                       (t nil))
                     (dolist (c children)
                       (scan c in-backquote)))))))
      (dolist (b body-nodes)
        (scan b t)))
    (nreverse unhygienic)))

(defun check-unhygienic-macro-binding (node path dialect)
  "Detect literal symbol bindings in backquoted let/let* forms inside defmacro."
  (declare (ignore dialect))
  (when (compound-node-p node)
    (let ((children (get-node-children node)))
      (when (and (>= (length children) 3)
                 (leaf-any-symbol-p (first children))
                 (member (leaf-symbol-name (first children)) '("defmacro" "defmacro*") :test #'string=))
        (let* ((body-nodes (cddr children))
               (unhygienic (find-unhygienic-macro-bindings body-nodes)))
          (when unhygienic
            (let ((first-un (first unhygienic)))
              (make-lint-finding
                :rule :unhygienic-macro-binding
                :path (getf first-un :path)
                :message (fmt "Unhygienic macro binding '~A': literal symbol in macro expansion risks variable capture. Use gensym or with-gensyms."
                              (getf first-un :name))
                :severity :warning
                :suggested-fix nil))))))))

(defun find-tail-self-calls (fn-name body-node)
  "Recursively search BODY-NODE for tail-position calls to FN-NAME in Clojure."
  (let ((hits '()))
    (labels ((scan-tail (n)
               (when (and n (compound-node-p n))
                 (let* ((children (get-node-children n))
                        (head (first children))
                        (head-name (when (and head (leaf-any-symbol-p head)) (leaf-symbol-name head))))
                   (cond
                     ((string= head-name fn-name)
                       (push (get-node-path n) hits))
                     ((member head-name '("if" "if-not") :test #'string=)
                       (when (third children) (scan-tail (third children)))
                       (when (fourth children) (scan-tail (fourth children))))
                     ((member head-name '("when" "when-not") :test #'string=)
                       (when (rest children) (scan-tail (first (last children)))))
                     ((equal head-name "do")
                       (when (rest children) (scan-tail (first (last children)))))
                     ((member head-name '("let" "loop") :test #'string=)
                       (when (cddr children) (scan-tail (first (last children)))))
                     ((equal head-name "cond")
                       (loop for (test-expr res-expr) on (rest children) by #'cddr
                             while test-expr
                             do (when res-expr (scan-tail res-expr))))
                     (t nil))))))
      (scan-tail body-node)
      hits)))

(defun check-clojure-tail-recur (node path dialect)
  "Detect recursive self-calls by function name in tail position in Clojure; suggest 'recur'."
  (declare (ignore dialect))
  (when (compound-node-p node)
    (let ((children (get-node-children node)))
      (when (and (>= (length children) 3)
                 (leaf-any-symbol-p (first children))
                 (member (leaf-symbol-name (first children)) '("defn" "defn-") :test #'string=))
        (let* ((name-node (second children))
               (fn-name (when (leaf-any-symbol-p name-node) (leaf-symbol-name name-node))))
          (when fn-name
            ;; Body starts after optional docstring / params vector
            (let ((body-tail (first (last children))))
              (when body-tail
                (let ((hits (find-tail-self-calls fn-name body-tail)))
                  (when hits
                    (make-lint-finding
                      :rule :clojure-tail-recur
                      :path (first hits)
                      :message (fmt "Self-call to '~A' in tail position risks stack overflow in Clojure; use 'recur' for tail-call optimization." fn-name)
                      :severity :warning
                      :suggested-fix (fmt "Replace (~A ...) with (recur ...)" fn-name))))))))))))

;;; ---------------------------------------------------------------------------
;;; P1 Rules: State & Scoping Hazards
;;; ---------------------------------------------------------------------------

(defparameter *literal-mutating-functions*
  '("nconc" "nreverse" "nreconc" "sort" "stable-sort" "delete" "delete-if"
    "set-car!" "set-cdr!" "vector-set!" "assoc!" "dissoc!" "conj!")
  "Functions that destructively modify data structures in place.")

(defun quoted-or-literal-node-p (node)
  "Return T if NODE is a quoted literal ('(...) or (quote ...))."
  (when (compound-node-p node)
    (let ((children (get-node-children node)))
      (when children
        (let ((head (first children)))
          (when (leaf-any-symbol-p head)
            (equal (leaf-symbol-name head) "quote")))))))

(defun check-mutate-literal-constant (node path dialect)
  "Detect destructive mutation of literal or quoted constants."
  (declare (ignore dialect))
  (when (compound-node-p node)
    (let ((children (get-node-children node)))
      (when children
        (let* ((head (first children))
               (head-name (when (leaf-any-symbol-p head) (leaf-symbol-name head))))
          (cond
            ;; (setf (car '(...)) val) or (setf (cdr '(...)) val)
            ((and (string= head-name "setf") (second children) (compound-node-p (second children)))
              (let* ((place (second children))
                     (place-children (get-node-children place)))
                (when (and place-children (leaf-any-symbol-p (first place-children)))
                  (let ((place-op (leaf-symbol-name (first place-children))))
                    (when (member place-op '("car" "cdr" "first" "rest" "nth" "aref") :test #'string=)
                      (loop for arg in (rest place-children)
                            when (or (quoted-or-literal-node-p arg)
                                     (and (leaf-any-symbol-p arg) (string= (leaf-symbol-name arg) "'")))
                            do (return (make-lint-finding
                                         :rule :mutate-literal-constant
                                         :path path
                                         :message "Modifying literal constant data has undefined behavior and can cause memory write faults."
                                         :severity :error
                                         :suggested-fix nil))))))))
            ;; (nconc '(1 2) '(3 4)) or (sort '(1 2) #'<)
            ((member head-name *literal-mutating-functions* :test #'string=)
              (loop for arg in (rest children)
                    when (or (quoted-or-literal-node-p arg)
                             (and (leaf-any-symbol-p arg) (string= (leaf-symbol-name arg) "'")))
                    do (return (make-lint-finding
                                 :rule :mutate-literal-constant
                                 :path path
                                 :message (fmt "Destructive operation '~A' called on quoted literal constant data." head-name)
                                 :severity :error
                                 :suggested-fix nil))))
            (t nil)))))))

(defun check-special-var-earmuffs (node path dialect)
  "Detect defvar or defparameter definitions lacking standard earmuffs (*...*)."
  (when (member dialect '(:common-lisp :emacs-lisp nil))
    (when (compound-node-p node)
      (let ((children (get-node-children node)))
        (when (and (>= (length children) 2)
                   (leaf-any-symbol-p (first children))
                   (member (leaf-symbol-name (first children)) '("defvar" "defparameter" "defcustom") :test #'string=))
          (let* ((var-node (second children))
                 (name (when (leaf-any-symbol-p var-node) (leaf-symbol-name var-node))))
            (when (and name (not (dynamic-variable-name-p name)))
              (make-lint-finding
                :rule :special-var-earmuffs
                :path path
                :message (fmt "Global variable '~A' lacks earmuffs (*...*). In Common Lisp, this makes the symbol globally special, polluting lexical scopes." name)
                :severity :style
                :suggested-fix (fmt "*~A*" name)))))))))

(defparameter *known-side-effect-operators*
  '("println" "print" "prn" "spit" "slurp" "send" "send-off" "future" "pmap")
  "Operators known to perform I/O or side-effects that should not be in retry transactions.")

(defun contains-side-effect-call-p (n)
  "Recursively check if N contains a call to a known side-effect function."
  (when n
    (if (compound-node-p n)
      (let* ((children (get-node-children n))
             (head (first children))
             (head-name (when (and head (leaf-any-symbol-p head)) (leaf-symbol-name head))))
        (or (member head-name *known-side-effect-operators* :test #'string=)
            (some #'contains-side-effect-call-p children)))
      nil)))

(defun check-clojure-swap-side-effects (node path dialect)
  "Detect side-effects (e.g. println, spit) inside STM/CAS retry forms (swap!, alter, dosync)."
  (declare (ignore dialect))
  (when (compound-node-p node)
    (let ((children (get-node-children node)))
      (when children
        (let* ((head (first children))
               (head-name (when (leaf-any-symbol-p head) (leaf-symbol-name head))))
          (when (member head-name '("swap!" "alter" "commute" "dosync" "reset-vals!") :test #'string=)
            (when (some #'contains-side-effect-call-p (rest children))
              (make-lint-finding
                :rule :clojure-swap-side-effects
                :path path
                :message (fmt "Side-effects detected inside '~A'; STM and atomic references retry on conflict, which will repeat side-effects." head-name)
                :severity :warning
                :suggested-fix nil))))))))

;;; ---------------------------------------------------------------------------
;;; P2 Rules: Idiomatic Traps & Dead Code
;;; ---------------------------------------------------------------------------

(defun check-dead-cond-clauses (node path dialect)
  "Detect dead clauses appearing after an unconditional default branch (t, otherwise, :else) in cond."
  (declare (ignore dialect))
  (when (compound-node-p node)
    (let ((children (get-node-children node)))
      (when (and (>= (length children) 2)
                 (leaf-any-symbol-p (first children))
                 (equal (leaf-symbol-name (first children)) "cond"))
        (let ((seen-default nil)
              (dead-clause-path nil))
          (dolist (clause (rest children))
            (when (compound-node-p clause)
              (let* ((clause-children (get-node-children clause))
                     (test-node (first clause-children)))
                (if seen-default
                  (unless dead-clause-path (setf dead-clause-path (get-node-path clause)))
                  (when (and test-node
                             (or (leaf-true-p test-node)
                                 (and (leaf-any-symbol-p test-node)
                                      (member (leaf-symbol-name test-node) '("t" "otherwise" ":else" "else") :test #'string=))))
                    (setf seen-default t))))))
          (when dead-clause-path
            (make-lint-finding
              :rule :dead-cond-clauses
              :path dead-clause-path
              :message "Unreachable clause in 'cond' follows an unconditional default test."
              :severity :warning
              :suggested-fix nil)))))))

(defun check-inappropriate-equality (node path dialect)
  "Detect comparison of numbers, strings, or characters using eq or eq?."
  (declare (ignore dialect))
  (when (compound-node-p node)
    (let ((children (get-node-children node)))
      (when (and (= (length children) 3)
                 (leaf-any-symbol-p (first children))
                 (member (leaf-symbol-name (first children)) '("eq" "eq?") :test #'string=))
        (let* ((arg1 (second children))
               (arg2 (third children))
               (v1 (when (eq (get-node-tag arg1) :leaf) (parse-node arg1)))
               (v2 (when (eq (get-node-tag arg2) :leaf) (parse-node arg2))))
          (declare (ignore v1 v2))
          (let ((val1 (get-node-leaf-value arg1))
                (val2 (get-node-leaf-value arg2)))
            (when (or (numberp val1) (numberp val2)
                      (stringp val1) (stringp val2)
                      (characterp val1) (characterp val2))
              (let ((replacement
                      (cond
                        ((or (numberp val1) (numberp val2))
                          (format nil "(= ~A ~A)" (sexp-to-string arg1) (sexp-to-string arg2)))
                        ((or (stringp val1) (stringp val2))
                          (format nil "(string= ~A ~A)" (sexp-to-string arg1) (sexp-to-string arg2)))
                        (t
                          (format nil "(equal ~A ~A)" (sexp-to-string arg1) (sexp-to-string arg2))))))
                (make-lint-finding
                  :rule :inappropriate-equality
                  :path path
                  :message "Comparing numbers, strings, or characters with 'eq' is implementation-dependent; use '=', 'string=', or 'equal'."
                  :severity :warning
                  :suggested-fix replacement)))))))))

(defun check-clojure-vector-contains (node path dialect)
  "Detect (contains? [...] key) where first argument is a vector literal; contains? tests indices, not values."
  (declare (ignore dialect))
  (when (compound-node-p node)
    (let ((children (get-node-children node)))
      (when (and (>= (length children) 2)
                 (leaf-any-symbol-p (first children))
                 (equal (leaf-symbol-name (first children)) "contains?"))
        (let ((coll-node (second children)))
          (when (and coll-node (eq (get-node-tag coll-node) :square))
            (make-lint-finding
              :rule :clojure-vector-contains
              :path path
              :message "'contains?' on a vector checks for numerical index presence, not value containment. Use (some #{val} vec) or convert to set."
              :severity :warning
              :suggested-fix nil)))))))

(defun check-elisp-lexical-binding (node path dialect)
  "Detect Emacs Lisp file lacking ';; -*- lexical-binding: t; -*-' in initial comments."
  (when (eq dialect :emacs-lisp)
    (when (and (eq (get-node-tag node) :file) (null path))
      (let* ((children (get-node-children node))
             (first-comment (find-if (lambda (c) (eq (get-node-tag c) :comment)) children)))
        (unless (and first-comment
                     (multiple-value-bind (cpath ctag txt) (parse-node first-comment)
                       (declare (ignore cpath ctag))
                       (and (stringp txt) (search "lexical-binding: t" txt :test #'char-equal))))
          (make-lint-finding
            :rule :elisp-missing-lexical-binding
            :path '()
            :message "Emacs Lisp file lacks ';; -*- lexical-binding: t; -*-' header; defaults to dynamic variable scoping."
            :severity :style
            :suggested-fix ";; -*- lexical-binding: t; -*-"))))))

(defclass lint-rule ()
  ((id :initarg :id :accessor rule-id :type keyword)
   (check :initarg :check :accessor rule-check)
   (dialects :initarg :dialects :accessor rule-dialects :initform nil)
   (description :initarg :description :accessor rule-description :initform ""))
  (:documentation "Base class for structural AST lint rules."))

(defgeneric rule-applicable-p (rule dialect)
  (:documentation "Return T if RULE is applicable to DIALECT."))

(defgeneric check-rule (rule node path dialect)
  (:documentation "Execute RULE on NODE at PATH for DIALECT. Return LINT-FINDING or NIL."))

(defmethod rule-applicable-p ((rule lint-rule) dialect)
  (let ((rule-dialects (rule-dialects rule)))
    (or (null rule-dialects) (null dialect) (member dialect rule-dialects))))

(defmethod rule-applicable-p ((rule list) dialect)
  "Backward compatibility method for plist-based rule specs."
  (let ((rule-dialects (getf rule :dialects)))
    (or (null rule-dialects) (null dialect) (member dialect rule-dialects))))

(defmethod check-rule ((rule lint-rule) node path dialect)
  (let ((fn (rule-check rule)))
    (when fn (funcall fn node path dialect))))

(defmethod check-rule ((rule list) node path dialect)
  "Backward compatibility method for plist-based rule specs."
  (let ((fn (getf rule :check)))
    (when fn (funcall fn node path dialect))))

(defun make-lint-rule (id check &key dialects description)
  "Construct a LINT-RULE instance."
  (make-instance 'lint-rule
                 :id id
                 :check check
                 :dialects dialects
                 :description (or description "")))

(defparameter *anti-pattern-rules*
  (list
    (make-lint-rule :if-progn-to-when #'check-if-progn-to-when :dialects '(:common-lisp :emacs-lisp :scheme))
    (make-lint-rule :if-nil-to-when #'check-if-nil-to-when :dialects '(:common-lisp :emacs-lisp :scheme :clojure))
    (make-lint-rule :if-not-to-unless #'check-if-not-to-unless :dialects '(:common-lisp :emacs-lisp :clojure))
    (make-lint-rule :invert-if-not #'check-invert-if-not :dialects nil)
    (make-lint-rule :single-clause-cond #'check-single-clause-cond :dialects '(:common-lisp :emacs-lisp :scheme :clojure))
    (make-lint-rule :if-boolean-redundant #'check-if-boolean-redundant :dialects nil)
    (make-lint-rule :redundant-progn #'check-redundant-progn :dialects '(:common-lisp :emacs-lisp))
    (make-lint-rule :nested-let #'check-nested-let :dialects '(:common-lisp :emacs-lisp :scheme))
    (make-lint-rule :equal-nil-to-null #'check-equal-nil-to-null :dialects '(:common-lisp :emacs-lisp))
    ;; P0 Rules
    (make-lint-rule :ignored-destructive-return #'check-ignored-destructive-return :dialects '(:common-lisp :emacs-lisp)
                    :description "Detect calls to destructive sequence functions where the return value is discarded.")
    (make-lint-rule :unhygienic-macro-binding #'check-unhygienic-macro-binding :dialects '(:common-lisp :emacs-lisp :scheme)
                    :description "Detect literal symbol bindings in backquoted let forms inside defmacro.")
    (make-lint-rule :clojure-tail-recur #'check-clojure-tail-recur :dialects '(:clojure)
                    :description "Detect recursive self-calls in tail position in Clojure; suggest 'recur'.")
    ;; P1 Rules
    (make-lint-rule :mutate-literal-constant #'check-mutate-literal-constant :dialects '(:common-lisp :scheme :clojure)
                    :description "Detect destructive mutation of literal or quoted constants.")
    (make-lint-rule :special-var-earmuffs #'check-special-var-earmuffs :dialects '(:common-lisp :emacs-lisp)
                    :description "Detect global variable definitions lacking standard earmuffs (*...*).")
    (make-lint-rule :clojure-swap-side-effects #'check-clojure-swap-side-effects :dialects '(:clojure)
                    :description "Detect side-effects inside STM/CAS retry transactions.")
    ;; P2 Rules
    (make-lint-rule :dead-cond-clauses #'check-dead-cond-clauses :dialects nil
                    :description "Detect dead clauses appearing after an unconditional default test in cond.")
    (make-lint-rule :inappropriate-equality #'check-inappropriate-equality :dialects '(:common-lisp :scheme)
                    :description "Detect comparison of numbers, strings, or characters using eq or eq?.")
    (make-lint-rule :clojure-vector-contains #'check-clojure-vector-contains :dialects '(:clojure)
                    :description "Detect 'contains?' on vector literals testing indices rather than values.")
    (make-lint-rule :elisp-missing-lexical-binding #'check-elisp-lexical-binding :dialects '(:emacs-lisp)
                    :description "Detect Emacs Lisp file lacking ';; -*- lexical-binding: t; -*-' header; defaults to dynamic variable scoping."))
  "Active structural anti-pattern and code smell lint rules.")

(defun rule-matches-dialect-p (rule dialect)
  "Return T if RULE applies to DIALECT."
  (rule-applicable-p rule dialect))

(defun rule-matches-filter-p (rule-or-id requested-rules)
  "Return T if RULE-OR-ID is permitted by REQUESTED-RULES (list of keywords or strings)."
  (if (null requested-rules)
    t
    (let ((id (cond ((typep rule-or-id 'lint-rule) (rule-id rule-or-id))
                    ((listp rule-or-id) (getf rule-or-id :id))
                    (t rule-or-id))))
      (member (string id) requested-rules :test #'string-equal))))

(defun lint-node (node path &key (dialect :common-lisp) rules)
  "Check a single NODE against active rules. Return a list of LINT-FINDING instances."
  (let ((findings '()))
    (dolist (r *anti-pattern-rules*)
      (when (and (rule-applicable-p r dialect)
                 (rule-matches-filter-p r rules))
        (let ((finding (check-rule r node path dialect)))
          (when finding
            (push finding findings)))))
    (nreverse findings)))

(defun lint-ast (tree &key path dialect rules)
  "Recursively lint TREE (or subtree at PATH) for structural code smells and anti-patterns.
Returns a list of LINT-FINDING instances."
  (let* ((*current-lint-tree* tree)
         (start-node (resolve-tree-scope tree path))
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
