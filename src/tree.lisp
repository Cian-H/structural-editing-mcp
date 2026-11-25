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
           :get-node-comment
           :compound-node-p
           :resolve-tree-scope
           :parse-node
           :node
           :leaf
           :comment
           :ast-equal-p
           :ensure-node-id
           :get-node-id-handle
           :compute-node-hash
           :node-short-hash
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

(defun get-node-comment (node)
  "Return the text of comment NODE, or NIL if it is not a comment."
  (match node
         ((comment _ text) text)
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
                       (let ((cur-id (gethash node *node-id-table*)))
                         (cond
                           ((and cur-id (or (null existing-id) (eql cur-id existing-id))) cur-id)
                           (t
                             (let
                                 ((id (or existing-id cur-id (incf *next-node-id*))))
                               (setf (gethash node *node-id-table*) id)
                               (unless (gethash id *id-node-table*) (setf (gethash id *id-node-table*) node))
                               id)))))))

(defun compute-node-hash (node)
  "Compute a deterministic 64-bit FNV-1a structural hash of NODE, ignoring :path metadata."
  (let ((h #xCBF29CE484222325))
    (declare (type (unsigned-byte 64) h))
    (labels
        ((mix-byte (cur b)
           (declare (type (unsigned-byte 64) cur)
                    (type (unsigned-byte 8) b)
                    (optimize (speed 3) (safety 0)))
           (logand #xFFFFFFFFFFFFFFFF (* (logxor cur b) #x100000001B3)))
         (mix-u32 (cur v)
           (declare (type (unsigned-byte 64) cur)
                    (type (unsigned-byte 32) v)
                    (optimize (speed 3) (safety 0)))
           (let*
               ((c1 (mix-byte cur (ldb (byte 8 0) v)))
                (c2 (mix-byte c1 (ldb (byte 8 8) v)))
                (c3 (mix-byte c2 (ldb (byte 8 16) v))))
             (mix-byte c3 (ldb (byte 8 24) v))))
         (mix-str (cur s)
           (declare (type (unsigned-byte 64) cur)
                    (type string s)
                    (optimize (speed 3) (safety 1)))
           (let ((c cur))
             (declare (type (unsigned-byte 64) c))
             (loop for i of-type fixnum from 0 below (length s)
                   for code of-type fixnum = (char-code (char s i))
                   do (setf c (mix-byte c (logand code #xFF)))
                   (when (> code #xFF) (setf c (mix-byte c (logand (ash code -8) #xFF)))))
             (mix-byte c #xFF)))
         (mix-val (cur val)
           (declare (type (unsigned-byte 64) cur))
           (cond
             ((and (consp val) (eq (car val) :raw) (stringp (cdr val)))
               (mix-str (mix-byte cur 10) (cdr val)))
             ((symbolp val)
               (let
                   ((c
                      (mix-byte cur
                                (if (keywordp val)
                                  12
                                  11))))
                 (mix-str c (symbol-name val))))
             ((stringp val) (mix-str (mix-byte cur 13) val))
             ((integerp val)
               (let ((c (mix-byte cur 14)))
                 (if (<= 0 val #xFFFFFFFF)
                   (mix-u32 c val)
                   (let
                       ((*print-readably* nil) (*print-pretty* nil))
                     (mix-str c (format nil "~S" val))))))
             (t
               (let
                   ((*print-readably* nil) (*print-pretty* nil))
                 (mix-str cur (format nil "~S" val))))))
         (walk (cur n)
           (declare (type (unsigned-byte 64) cur))
           (if (and (consp n) (eq (first n) :path))
             (let ((tag (third n)))
               (cond
                 ((eq tag :leaf) (mix-val (mix-byte cur 1) (fourth n)))
                 ((eq tag :comment)
                   (let ((c (mix-byte cur 2)) (txt (fourth n)))
                     (if (stringp txt)
                       (mix-str c txt)
                       c)))
                 ((keywordp tag)
                   (let
                       ((c (mix-u32 (mix-byte cur 3) (logand #xFFFFFFFF (sxhash tag))))
                        (children (cdddr n))
                        (len 0))
                     (declare (type (unsigned-byte 64) c) (type fixnum len))
                     (dolist (child children)
                       (incf len)
                       (setf c (walk c child)))
                     (mix-byte c (logand len #xFF))))
                 (t (mix-val (mix-byte cur 4) n))))
             (mix-val (mix-byte cur 4) n))))
      (declare (inline mix-byte mix-u32))
      (setf h (walk h node))
      h)))

(defun node-short-hash (node)
  "Return a 4-character lowercase hex structural hash for NODE."
  (let*
      ((h (logand (compute-node-hash node) #xFFFF))
       (digits "0123456789abcdef")
       (out (make-string 4 :element-type 'character)))
    (declare (type (unsigned-byte 16) h))
    (setf (char out 0)
          (char digits (ldb (byte 4 12) h))
          (char out 1)
          (char digits (ldb (byte 4 8) h))
          (char out 2)
          (char digits (ldb (byte 4 4) h))
          (char out 3)
          (char digits (ldb (byte 4 0) h)))
    out))

(defun get-node-id-handle (node)
  "Return the Hybrid EASE hashnode handle string (e.g. \"#n402:3a9f\") for NODE."
  (let ((id (ensure-node-id node)))
    (if id
      (format nil "#n~D:~A" id (node-short-hash node))
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
  "Resolve SELECTOR (integer list, \"#n402\" or \"#n402:a3f9\" handle, or symbolic segment list like (\"preview.lisp\" \"defun:foo\" 2)) to a canonical integer path list in TREE."
  (labels
      ((hex-string-p (s)
         (and (plusp (length s))
              (every
                (lambda (c)
                  (digit-char-p c 16))
                s)))
       (parse-handle-id (str)
         (when (stringp str)
           (let*
               ((trimmed (string-trim " " str))
                (colon-pos (position #\: trimmed))
                (id-part
                  (if colon-pos
                    (subseq trimmed 0 colon-pos)
                    trimmed))
                (hash-part (when colon-pos (subseq trimmed (1+ colon-pos)))))
             (when
                 (or (null hash-part) (hex-string-p hash-part))
               (cond
                 ((and (> (length id-part) 2)
                       (string-equal "#n" id-part :end2 2)
                       (every #'digit-char-p (subseq id-part 2)))
                   (values (parse-integer (subseq id-part 2)) hash-part))
                 ((and (> (length id-part) 1)
                       (char-equal (char id-part 0) #\n)
                       (every #'digit-char-p (subseq id-part 1)))
                   (values (parse-integer (subseq id-part 1)) hash-part))
                 (t (values nil nil)))))))
       (find-node-and-path-by-id
           (node target-id rev-path)
         (when (and (null rev-path) (consp node))
           (let
               ((cached
                  (bt:with-lock-held (*node-id-lock*) (gethash target-id *id-node-table*))))
             (when cached
               (let ((cpath (get-node-path cached)))
                 (when
                     (and cpath (eq (get-node-at-path node cpath) cached))
                   (return-from find-node-and-path-by-id (values cached cpath)))))))
         (when (consp node)
           (if
               (eql (gethash node *node-id-table*) target-id)
             (values node (or (get-node-path node) (reverse rev-path)))
             (loop for child in (get-node-children node)
                   for idx from 0
                   do
                   (multiple-value-bind (found-node found-path)
                                        (find-node-and-path-by-id child target-id (cons idx rev-path))
                     (when found-node (return (values found-node found-path))))))))
       (resolve-id
           (target-id &optional expected-hash raw-handle)
         (multiple-value-bind (found-node found-path)
                              (find-node-and-path-by-id tree target-id '())
           (when found-node
             (when expected-hash
               (let ((actual-hash (node-short-hash found-node)))
                 (unless (string-equal actual-hash expected-hash)
                   (error 'hash-mismatch-error
                          :handle
                          (or raw-handle (format nil "#n~D:~A" target-id expected-hash))
                          :expected-hash
                          expected-hash
                          :actual-hash
                          actual-hash
                          :target-path
                          found-path))))
             found-path)))
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
        (multiple-value-bind (hid hhash)
                             (parse-handle-id selector)
          (if hid
            (resolve-id hid hhash selector)
            (resolve-symbolic-path tree (list selector)))))
      ((listp selector)
        (loop with curr = tree
              with acc = '()
              for seg in selector
              do
              (cond
                ((null curr) (return nil))
                ((integerp seg)
                  (let ((children (get-node-children curr)))
                    (if
                        (and children (<= 0 seg) (< seg (length children)))
                      (setf curr (nth seg children) acc (append acc (list seg)))
                      (return nil))))
                ((stringp seg)
                  (multiple-value-bind (hid hhash)
                                       (parse-handle-id seg)
                    (if hid
                      (let ((hpath (resolve-id hid hhash seg)))
                        (if hpath
                          (setf acc hpath curr (get-node-at-path tree hpath))
                          (return nil)))
                      (let ((tag (get-node-tag curr)))
                        (cond
                          ((eq tag :workspace)
                            (multiple-value-bind (found-node found-path)
                                                 (block find-file
                                                   (loop for d-node in (get-node-children curr)
                                                         for d-idx from 0
                                                         do
                                                         (loop for f-node in (get-node-children d-node)
                                                               for f-idx from 0
                                                               for cpath = (list d-idx f-idx)
                                                               when (file-matches-p cpath seg)
                                                               do (return-from find-file (values f-node cpath)))))
                              (if found-node
                                (setf curr found-node acc found-path)
                                (return nil))))
                          ((and
                             (not (member tag '(:file :paren :square :curly :set :leaf :comment)))
                             *symbolic-file-resolver*)
                            (multiple-value-bind (found-node found-idx)
                                                 (loop for f-node in (get-node-children curr)
                                                       for f-idx from 0
                                                       when (file-matches-p (append acc (list f-idx)) seg) return (values f-node f-idx))
                              (if found-node
                                (setf curr found-node acc (append acc (list found-idx)))
                                (return nil))))
                          (t
                            (multiple-value-bind (found-node found-idx)
                                                 (loop for c-node in (get-node-children curr)
                                                       for c-idx from 0
                                                       when (form-matches-p c-node seg) return (values c-node c-idx))
                              (if found-node
                                (setf curr found-node acc (append acc (list found-idx)))
                                (return nil)))))))))
                (t (return nil)))
              finally (return (or acc '(:root)))))
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
  (bt:with-lock-held (*node-id-lock*)
                     (let
                         ((has-ids-p (plusp (hash-table-count *node-id-table*))))
                       (labels
                           ((reindex-aux (node rev-path)
                              (if (and (consp node) (eq (first node) :path))
                                (let*
                                    ((full-path (reverse rev-path))
                                     (old-id (when has-ids-p (gethash node *node-id-table*)))
                                     (tag (third node))
                                     (new-node
                                       (cond
                                         ((eq tag :leaf) (list :path full-path :leaf (fourth node)))
                                         ((eq tag :comment) (list :path full-path :comment (fourth node)))
                                         ((keywordp tag)
                                           (list* :path
                                                  full-path
                                                  tag
                                                  (loop for child in (cdddr node)
                                                        for idx of-type fixnum from 0
                                                        collect (reindex-aux child (cons idx rev-path)))))
                                         (t node))))
                                  (when (and old-id (consp new-node))
                                    (setf (gethash new-node *node-id-table*) old-id)
                                    (unless (gethash old-id *id-node-table*)
                                      (setf (gethash old-id *id-node-table*) new-node)))
                                  new-node)
                                node)))
                         (reindex-aux tree (reverse current-path))))))

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
                       (let
                           ((old-id (gethash elem *node-id-table*))
                            (new-node
                              (list* :path
                                     node-path
                                     tag
                                     (loop for child in children
                                           for i of-type fixnum from 0
                                           collect
                                           (if (= i child-idx)
                                             (walk child (cdr p) (append curr-path (list i)))
                                             child)))))
                         (when old-id (ensure-node-id new-node old-id))
                         new-node))))
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
