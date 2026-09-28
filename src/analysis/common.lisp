(in-package :structural-editing-mcp.analysis)

(declaim (optimize (speed 2) (safety 3)))

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

(defun count-ast-nodes (node)
  "Count total number of nodes (forms and leaves) in NODE."
  (let ((count 1))
    (dolist (c (get-node-children node))
      (incf count (count-ast-nodes c)))
    count))

(defun compute-nesting-depth (node &optional (current-depth 0))
  "Compute the maximum parenthetical nesting depth of sub-expressions within NODE."
  (let ((children (get-node-children node)))
    (if (and (member (get-node-tag node) '(:paren :square :curly))
             children)
      (reduce #'max children
              :key (lambda (c) (compute-nesting-depth c (1+ current-depth)))
              :initial-value (1+ current-depth))
      current-depth)))

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
