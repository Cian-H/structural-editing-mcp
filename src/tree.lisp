(defpackage :structural-editing-mcp.tree
  (:use :cl :trivia :alexandria :structural-editing-mcp.conditions)
  (:import-from :serapeum :fmt)
  (:export :get-node-at-path
           :reindex-paths
           :update-node-at-path
           :get-node-path
           :get-node-tag
           :get-node-children
           :get-node-leaf-value
           :compound-node-p
           :resolve-tree-scope
           :parse-node
           :node
           :leaf
           :comment
           :ast-equal-p
           :ensure-node-id
           :get-node-id-handle
           :resolve-symbolic-path
           :*symbolic-file-resolver*
           :*prefix-node-tags*
           :prefix-tag-p
           :prefix-tag-string
           :raw-atom-p
           :raw-atom-text
           :atom-symbol-name)
  (:documentation
    "Core AST representations, patterns, accessors, and path navigation."))

(in-package :structural-editing-mcp.tree)

(defpattern node (path tag children)
            `(list* :path ,path ,tag ,children))

(defpattern leaf (path val)
            `(list :path ,path :leaf ,val))

(defpattern comment (path text)
            `(list :path ,path :comment ,text))

(declaim (inline get-node-path get-node-tag get-node-children compound-node-p))

(defun get-node-path (node)
  "Return the path list of NODE, or NIL if invalid."
  (match node
         ((or (node path _ _) (leaf path _) (comment path _)) path)
         (_ nil)))

(defun get-node-tag (node)
  "Return the tag keyword (:paren, :leaf, :comment, :file, :workspace, etc.) of NODE."
  (match node
         ((leaf _ _) :leaf)
         ((comment _ _) :comment)
         ((node _ tag _) tag)
         (_ nil)))

(defun get-node-children (node)
  "Return the children of collection NODE, or NIL if it is a leaf or invalid."
  (match node
         ((or (leaf _ _) (comment _ _)) nil)
         ((node _ _ children) children)
         (_ nil)))

(defun get-node-leaf-value (node)
  "Return the value of leaf NODE, or NIL if it is not a leaf."
  (match node
         ((leaf _ val) val)
         (_ nil)))

(defun compound-node-p (node)
  "Return T if NODE is a compound parenthesized or bracketed collection."
  (let ((tag (get-node-tag node)))
    (or (eq tag :paren) (eq tag :square))))

(defparameter *prefix-node-tags*
  '((:quote . "'")
    (:quasiquote . "`")
    (:unquote . ",")
    (:unquote-splicing . ",@")
    (:syntax-unquote . "~")
    (:syntax-unquote-splicing . "~@")
    (:deref . "@")
    (:function . "#'")
    (:discard . "#_")
    (:sharp . "#")
    (:reader-conditional . "#?")
    (:reader-conditional-splicing . "#?@"))
  "Alist mapping reader-macro prefix node tags to their exact source prefix strings.
A prefix node has the shape (:path P TAG CHILD) and prints as PREFIX immediately followed by CHILD,
so that e.g. '(a b) is a single addressable node rather than two siblings.")

(defun prefix-tag-p (tag)
  "Return T if TAG denotes a reader-macro prefix node (e.g. :quote)."
  (and (assoc tag *prefix-node-tags*) t))

(defun prefix-tag-string (tag)
  "Return the source prefix string for prefix node TAG (e.g. \"'\" for :quote), or NIL."
  (cdr (assoc tag *prefix-node-tags*)))

(defun raw-atom-p (val)
  "Return T if leaf value VAL is a verbatim (:raw . text) atom."
  (and (consp val) (eq (car val) :raw) (stringp (cdr val))))

(defun raw-atom-text (val)
  "Return the verbatim source text of raw atom VAL, or NIL."
  (when (raw-atom-p val) (cdr val)))

(defun raw-symbol-text-p (text)
  "Return T if raw atom TEXT looks like a (non-keyword) symbol rather than a number, string or character."
  (let ((len (length text)))
    (and (plusp len)
         (let ((c0 (char text 0)) (c1 (and (> len 1) (char text 1))))
           (not (or (member c0 '(#\" #\# #\\ #\:))
                    (digit-char-p c0)
                    (and (member c0 '(#\+ #\- #\.)) c1 (digit-char-p c1))))))))

(defun atom-symbol-name (val)
  "Return the symbol name of leaf value VAL: SYMBOL-NAME for symbols, the verbatim text for
symbol-like raw atoms (e.g. case-sensitive identifiers such as Foo), otherwise NIL."
  (cond
    ((symbolp val) (symbol-name val))
    ((raw-atom-p val) (let ((text (cdr val))) (when (raw-symbol-text-p text) text)))
    (t nil)))

(defun parse-node (node)
  "Destructure NODE and return (values path tag children-or-leaf-val)."
  (match node
         ((leaf path val) (values path :leaf val))
         ((comment path text) (values path :comment text))
         ((node path tag children) (values path tag children))
         (_ (values nil nil nil))))

(defvar *node-id-lock* (bt:make-lock "node-id-lock")
  "Lock protecting node ID allocation and table registration.")

(defvar *next-node-id* 0
  "Monotonic counter for assigning Hybrid \"Explicitly Addressed Sequence Encoding\" (EASE) handles (#n<id>) to AST nodes.")

(defvar *node-id-table*
  (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Weak hash table mapping AST node cons cells to their monotonic integer ID.")

(defvar *id-node-table*
  (make-hash-table :test 'eql :weakness :value :synchronized t)
  "Weak hash table mapping monotonic integer node IDs to their current AST node.")

(defvar *symbolic-file-resolver* nil
  "Optional hook (lambda (path) -> filepath-string) registered by the workspace package.")

(defun ensure-node-id
       (node &optional existing-id)
  "Return the monotonic integer ID for NODE, assigning one if not yet registered."
  (when (consp node)
    (bt:with-lock-held (*node-id-lock*)
      (let
          ((id (or existing-id (gethash node *node-id-table*) (incf *next-node-id*))))
        (setf (gethash node *node-id-table*) id)
        (setf (gethash id *id-node-table*) node)
        id))))

(defun get-node-id-handle (node)
  "Return the Hybrid EASE handle string (e.g. \"#n402\") for NODE."
  (let ((id (ensure-node-id node)))
    (if id
      (format nil "#n~D" id)
      "")))

(defun node-atom-name (node)
  "Extract a comparable string name from a :leaf node or the head leaf of a compound node."
  (multiple-value-bind (path tag val)
                       (parse-node node)
    (declare (ignore path))
    (cond
      ((eq tag :leaf)
        (string-trim "\""
                     (cond
                       ((symbolp val) (symbol-name val))
                       ((stringp val) val)
                       ((and (consp val) (eq (car val) :sym)) (cadr val))
                       ((and (consp val) (stringp (cdr val))) (cdr val))
                       (t (princ-to-string val)))))
      ((and (member tag '(:paren :square)) (consp val)) (node-atom-name (first val)))
      (t nil))))

(defun resolve-symbolic-path (tree selector)
  "Resolve SELECTOR (integer list, \"#n402\" handle, or symbolic segment list like (\"preview.lisp\" \"defun:foo\" 2)) to a canonical integer path list in TREE."
  (labels
      ((parse-handle-id (str)
         (when (stringp str)
           (let ((trimmed (string-trim " " str)))
             (cond
               ((and (> (length trimmed) 2)
                     (string-equal "#n" trimmed :end2 2)
                     (every #'digit-char-p (subseq trimmed 2)))
                 (parse-integer (subseq trimmed 2)))
               ((and (> (length trimmed) 1)
                     (char-equal (char trimmed 0) #\n)
                     (every #'digit-char-p (subseq trimmed 1)))
                 (parse-integer (subseq trimmed 1)))
               (t nil)))))
       (find-path-by-id (node target-id rev-path)
         (when (consp node)
           (if
               (eql (gethash node *node-id-table*) target-id)
             (or (get-node-path node) (reverse rev-path))
             (loop for child in
                   (get-node-children node) for idx from 0 for found
                   =
                   (find-path-by-id child target-id (cons idx rev-path))
                   when found return found))))
       (resolve-id (target-id)
         (find-path-by-id tree target-id '()))
       (file-matches-p (coord-path seg)
         (when *symbolic-file-resolver*
           (let
               ((fp (funcall *symbolic-file-resolver* coord-path)))
             (and (stringp fp)
                  (or (string= fp seg)
                      (and (>= (length fp) (length seg))
                           (string= fp seg :start1 (- (length fp) (length seg)))
                           (or (= (length fp) (length seg))
                               (char= (char fp (- (length fp) (length seg) 1)) #\/))))))))
       (form-matches-p (child seg)
         (let*
             ((non-comments (remove :comment (get-node-children child) :key #'get-node-tag))
              (head-name (when non-comments (node-atom-name (first non-comments))))
              (sec-name (when (second non-comments) (node-atom-name (second non-comments))))
              (colon-pos (position #\: seg)))
           (if (and colon-pos (plusp colon-pos))
             (let
                 ((op (subseq seg 0 colon-pos)) (nm (subseq seg (1+ colon-pos))))
               (and head-name (string-equal head-name op) sec-name (string-equal sec-name nm)))
             (or (and sec-name (string-equal sec-name seg))
                 (and head-name (string-equal head-name seg)))))))
    (cond
      ((null selector) nil)
      ((and (listp selector) (every #'integerp selector)) selector)
      ((stringp selector)
        (let ((hid (parse-handle-id selector)))
          (if hid
            (resolve-id hid)
            (resolve-symbolic-path tree (list selector)))))
      ((listp selector)
        (loop with curr = tree with acc = '
              () for seg in selector do
              (cond
                ((null curr) (return nil))
                ((integerp seg)
                  (let ((children (get-node-children curr)))
                    (if
                        (and children (<= 0 seg) (< seg (length children)))
                      (setf curr (nth seg children) acc (append acc (list seg)))
                      (return nil))))
                ((and (stringp seg) (parse-handle-id seg))
                  (let ((hpath (resolve-id (parse-handle-id seg))))
                    (if hpath
                      (setf acc hpath curr (get-node-at-path tree hpath))
                      (return nil))))
                ((stringp seg)
                  (let ((tag (get-node-tag curr)))
                    (cond
                      ((eq tag :workspace)
                        (multiple-value-bind (found-node found-path)
                            (block find-file
                              (loop for d-node in
                                    (get-node-children curr) for d-idx from 0 do
                                    (loop for f-node in
                                          (get-node-children d-node) for f-idx from 0 for
                                          cpath =
                                          (list d-idx f-idx) when
                                          (file-matches-p cpath seg) do
                                          (return-from find-file (values f-node cpath)))))
                          (if found-node
                            (setf curr found-node acc found-path)
                            (return nil))))
                      ((and
                         (not (member tag '(:file :paren :square :curly :set :leaf :comment)))
                         *symbolic-file-resolver*)
                        (multiple-value-bind (found-node found-idx)
                                             (loop for f-node in
                                                   (get-node-children curr) for f-idx from 0 when
                                                   (file-matches-p (append acc (list f-idx)) seg)
                                                   return
                                                   (values f-node f-idx))
                          (if found-node
                            (setf curr found-node acc (append acc (list found-idx)))
                            (return nil))))
                      (t
                        (multiple-value-bind (found-node found-idx)
                                             (loop for c-node in
                                                   (get-node-children curr) for c-idx from 0 when
                                                   (form-matches-p c-node seg) return
                                                   (values c-node c-idx))
                          (if found-node
                            (setf curr found-node acc (append acc (list found-idx)))
                            (return nil)))))))
                (t (return nil)))
              finally
              (return (or acc '(:root)))))
      (t nil))))

(defun get-node-at-path (tree path)
  "Navigate to the node at PATH in TREE, or NIL if not found. Supports integer lists, #n<id> handles, and symbolic path segments."
  (let
      ((resolved
         (if (and (listp path) (every #'integerp path))
           path
           (resolve-symbolic-path tree path))))
    (when (and resolved (listp resolved))
      (if (equal resolved '(:root))
        tree
        (let*
            ((tree-path (get-node-path tree))
             (steps
               (if
                   (and tree-path
                        (>= (length resolved) (length tree-path))
                        (equal tree-path (subseq resolved 0 (length tree-path))))
                 (subseq resolved (length tree-path))
                 resolved)))
          (loop with current = tree for idx in steps for
                children =
                (get-node-children current) for sub =
                (and (integerp idx) children (>= idx 0) (nthcdr idx children))
                if sub do
                (setf current (car sub)) else return nil finally
                (return current)))))))

(defun resolve-tree-scope (tree path)
  "Resolve PATH (integer coordinate list, #n<id> handle, or symbolic selector list) against TREE."
  (if (or (null path) (equal path ""))
    tree
    (let
        ((resolved (resolve-symbolic-path tree path)))
      (when resolved
        (if (equal resolved '(:root))
          tree
          (get-node-at-path tree resolved))))))

(defun reindex-paths
       (tree &optional (current-path '()))
  "Recompute and update all :path metadata in TREE starting at CURRENT-PATH, preserving Hybrid EASE node IDs."
  (declare (type list current-path))
  (labels
      ((reindex-aux (node rev-path)
         (let
             ((full-path (reverse rev-path))
              (old-id (when (consp node) (gethash node *node-id-table*))))
           (let
               ((new-node
                  (match node
                         ((leaf _ val) (list :path full-path :leaf val))
                         ((comment _ text) (list :path full-path :comment text))
                         ((node _ tag children)
                          (list* :path
                                 full-path
                                 tag
                                 (loop for child in children for idx of-type
                                       fixnum from 0 collect
                                       (reindex-aux child (cons idx rev-path)))))
                         (_ node))))
             (when (consp new-node) (ensure-node-id new-node old-id))
             new-node))))
    (reindex-aux tree (reverse current-path))))

(defun update-node-at-path (tree path fn)
  "Navigate to PATH in TREE, apply FN to the node at PATH, and reconstruct the tree.
Only re-indexes the modified subtree at PATH rather than traversing the entire tree root."
  (declare (type list path))
  (labels
      ((walk (elem p curr-path)
         (if (null p)
           (reindex-paths (funcall fn elem) curr-path)
           (match elem
                  ((node node-path tag children)
                   (let ((child-idx (car p)))
                     (if
                         (or (< child-idx 0) (>= child-idx (length children)))
                       (error 'invalid-path-error
                              :path
                              path
                              :tree
                              tree
                              :message
                              (fmt "Child index ~D out of bounds for node at path ~A" child-idx node-path))
                       (list* :path
                              node-path
                              tag
                              (loop for child in children for i of-type fixnum
                                    from 0 collect
                                    (if (= i child-idx)
                                      (walk child (cdr p) (append curr-path (list i)))
                                      child))))))
                  (_
                    (error 'invalid-path-error
                           :path
                           path
                           :tree
                           tree
                           :message
                           (fmt "Cannot navigate into non-node element at path ~A" curr-path)))))))
    (walk tree path '())))

(defun ast-equal-p (node-a node-b)
  "Return T if NODE-A and NODE-B are structurally identical AST nodes, ignoring :path metadata."
  (or (eq node-a node-b)
      (multiple-value-bind (path-a tag-a children-a)
                           (parse-node node-a)
        (declare (ignore path-a))
        (multiple-value-bind (path-b tag-b children-b)
                             (parse-node node-b)
          (declare (ignore path-b))
          (if (and tag-a tag-b)
            (and (eq tag-a tag-b)
                 (if (or (eq tag-a :leaf) (eq tag-a :comment))
                   (equal children-a children-b)
                   (and (= (length children-a) (length children-b))
                        (every #'ast-equal-p children-a children-b))))
            (and (null tag-a) (null tag-b) (equal node-a node-b)))))))
