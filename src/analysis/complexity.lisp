(in-package :structural-editing-mcp.analysis)

(declaim (optimize (speed 2) (safety 3)))

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
    ((member name '("IF" "WHEN" "UNLESS" "WHEN-NOT" "IF-NOT" "WHEN-LET" "IF-LET" "WHEN-FIRST"
                    "LOOP" "DOLIST" "DOTIMES" "DO" "DO*" "DOSEQ" "RECUR")
             :test #'string=)
      1)
    ((string= name "COND")
      (count-cond-branch-clauses (rest children) dialect))
    ((member name '("CASE" "CCASE" "ECASE" "TYPECASE" "CTYPECASE" "ETYPECASE" "CONDP")
             :test #'string=)
      (count-case-branch-clauses (nthcdr 2 children) dialect))
    ((member name '("AND" "OR") :test #'string=)
      (max 0 (- (length children) 2)))
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
