(in-package :structural-editing-mcp.analysis)

(declaim (optimize (speed 2) (safety 3)))

(defstruct (duplicate-group (:constructor make-duplicate-group))
  code-snippet
  occurrence-count
  paths
  node-count
  depth
  recommendation)

(defun combine-hashes (h1 h2)
  "Combine two 64-bit integer hashes."
  (declare (type (unsigned-byte 64) h1 h2))
  (logand #xFFFFFFFFFFFFFFFF
          (logxor h1
                  (+ h2
                     #x9e3779b97f4a7c15
                     (ash (logand h1 #x03FFFFFFFFFFFFFF) 6)
                     (ash (logand h1 #xFFFFFFFFFFFFFFFF) -2)))))

(defun compute-structural-hash (node &key (exact t) (is-head t))
  "Compute a Merkle-style 64-bit integer structural hash of NODE."
  (match node
         ((leaf _ val)
          (if (or exact is-head)
            (let ((base-hash (sxhash val)))
              (combine-hashes (sxhash (type-of val)) (logand base-hash #xFFFFFFFFFFFFFFFF)))
            (combine-hashes (sxhash :anonymized-leaf) (sxhash '?_))))
         ((comment _ text)
          (if exact
            (combine-hashes (sxhash :comment) (logand (sxhash text) #xFFFFFFFFFFFFFFFF))
            (sxhash :comment)))
         ((node _ tag children)
          (let ((h (combine-hashes (sxhash :node) (sxhash tag))))
            (loop for c in children
                  for idx of-type fixnum from 0
                  do (setf h (combine-hashes h (compute-structural-hash c :exact exact :is-head (zerop idx)))))
            h))
         (_ (sxhash node))))

(defun canonicalize-subtree (node &key (exact t))
  "Produce a canonical string fingerprint of NODE for equality/clone matching via structural Merkle hashing."
  (format nil "~16,'0X" (compute-structural-hash node :exact exact :is-head t)))

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
      (when (evenp idx) :binding-clause))
    ((eq parent-context :binding-list)
      :binding-clause)
    ((eq parent-context :binding-clause)
      nil)
    ((or (member head-str *binding-form-heads* :test #'string=)
         (member head-str '("MULTIPLE-VALUE-BIND" "DESTRUCTURING-BIND") :test #'string=)
         (loop-with-vector-bindings-p head-str children))
      (when (= idx 1) :binding-list))
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
  (filter-map
    (lambda (fingerprint)
      (let ((entries (gethash fingerprint buckets)))
        (when (>= (length entries) 2)
          (let* ((meta (gethash fingerprint node-metadata))
                 (paths (mapcar #'car entries)))
            (list :fingerprint fingerprint
                  :paths (nreverse paths)
                  :node-count (getf meta :node-count)
                  :depth (getf meta :depth)
                  :sample (getf meta :sample))))))
    (hash-table-keys buckets)))

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
         (cleaned (trim-whitespace raw-str)))
    (ellipsize (substitute #\Space #\Newline cleaned) 80)))

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
