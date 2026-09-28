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
