(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

;;; Node Presentation & Discovery Helpers

(defun truncate-preview-string
       (raw-str &key (max-length 70))
  "Format RAW-STR into a single line trimmed of newlines, capped at MAX-LENGTH with ellipsis."
  (ellipsize
    (substitute #\space #\newline (trim-whitespace (or raw-str "")))
    max-length))

(defparameter *skeleton-threshold-children* 50
  "Default child count above which format-node-preview uses skeleton mode.")

(defparameter *skeleton-threshold-chars* 3000
  "Default character length above which format-node-preview uses skeleton mode.")

(defun count-subtree-nodes (node)
  "Recursively count total AST nodes under NODE."
  (if (null node)
    0
    (let ((children (structural-editing-mcp.tree:get-node-children node)))
      (if (null children)
        1
        (1+ (reduce #'+ children :key #'count-subtree-nodes :initial-value 0))))))

(defun format-child-stub (c)
  (let ((ctag (structural-editing-mcp.tree:get-node-tag c)))
    (cond
      ((or (eq ctag :leaf) (eq ctag :comment))
        (structural-editing-mcp.parser:sexp-to-string c))
      ((eq ctag :bracket) "[...]")
      ((eq ctag :brace) "{...}")
      (t "(...)"))))

(defun extract-node-signature
       (node &key (max-elements 3) (max-length 60))
  "Extract a compact signature/stub string for a node without hydrating its entire body."
  (if (null node)
    ""
    (let ((tag (structural-editing-mcp.tree:get-node-tag node)))
      (cond
        ((or (eq tag :leaf) (eq tag :comment))
          (truncate-preview-string
            (structural-editing-mcp.parser:sexp-to-string node)
            :max-length max-length))
        (t
          (let ((children (structural-editing-mcp.tree:get-node-children node))
                (prefix (case tag (:bracket "[") (:brace "{") (t "(")))
                (suffix (case tag (:bracket "]") (:brace "}") (t ")"))))
            (if (null children)
              (concatenate 'string prefix suffix)
              (let* ((count (min (length children) max-elements))
                     (head (subseq children 0 count))
                     (has-more (> (length children) max-elements))
                     (items (mapcar #'format-child-stub head))
                     (content (format nil "~{~A~^ ~}" items))
                     (res (if has-more
                            (format nil "~A~A ...~A" prefix content suffix)
                            (format nil "~A~A~A" prefix content suffix))))
                (truncate-preview-string res :max-length max-length)))))))))

(defun print-children-tree
       (s node current-depth max-depth base-path &key skeleton)
  (when (and (< current-depth max-depth)
             (not (member (structural-editing-mcp.tree:get-node-tag node) '(:leaf :comment))))
    (let ((children (structural-editing-mcp.tree:get-node-children node)))
      (when children
        (loop for child in children
              for idx from 0
              for cpath = (or (structural-editing-mcp.tree:get-node-path child)
                              (append base-path (list idx)))
              for ctag = (structural-editing-mcp.tree:get-node-tag child)
              for snippet = (if skeleton
                              (extract-node-signature child)
                              (truncate-preview-string (structural-editing-mcp.parser:sexp-to-string child)))
              for indent = (make-string (* (1+ current-depth) 2) :initial-element #\space)
              do
              (format s "~A[~{~A~^, ~}] ~A: ~A~%" indent cpath ctag snippet)
              (print-children-tree s child (1+ current-depth) max-depth cpath :skeleton skeleton))))))

(defun format-children-preview
       (s children path depth &optional (label-suffix "") &key (limit 50) (offset 0) skeleton)
  "Format child nodes with paths, tags, and preview snippets up to DEPTH with optional pagination and skeleton mode."
  (when children
    (let* ((total (length children))
           (start (min (max 0 (or offset 0)) total))
           (effective-limit (or limit 50))
           (end (min (+ start effective-limit) total))
           (slice (subseq children start end)))
      (if (or (plusp start) (< end total))
        (format s "~%Children (~A~A, showing ~A-~A):~%" total label-suffix (1+ start) end)
        (format s "~%Children (~A~A):~%" total label-suffix))
      (loop for child in slice
            for idx from start
            for cpath = (or (structural-editing-mcp.tree:get-node-path child) (append path (list idx)))
            for ctag = (structural-editing-mcp.tree:get-node-tag child)
            for snippet = (if skeleton
                            (extract-node-signature child)
                            (truncate-preview-string (structural-editing-mcp.parser:sexp-to-string child)))
            do
            (format s "  [~{~A~^, ~}] ~A: ~A~%" cpath ctag snippet)
            (when (> depth 1)
              (print-children-tree s child 1 depth cpath :skeleton skeleton)))
      (when (< end total)
        (format s "  ... (~A more child items; use limit and offset in read_node to paginate)~%"
                (- total end))))))

(defun format-dialect-node-preview
       (s dialect-node d-path &key (limit 50) (offset 0))
  "Format preview for a single DIALECT-NODE under D-PATH to stream S with optional pagination."
  (let* ((d-tag (structural-editing-mcp.tree:get-node-tag dialect-node))
         (file-nodes (structural-editing-mcp.tree:get-node-children dialect-node))
         (total (length file-nodes))
         (start (min (max 0 (or offset 0)) total))
         (effective-limit (or limit 50))
         (end (min (+ start effective-limit) total))
         (slice (subseq file-nodes start end)))
    (if (or (plusp start) (< end total))
      (format s "  [~{~A~^, ~}] ~A (~A file~:P, showing ~A-~A):~%" d-path d-tag total (1+ start) end)
      (format s "  [~{~A~^, ~}] ~A (~A file~:P):~%" d-path d-tag total))
    (loop for file-node in slice
          for f-idx from start
          for f-path = (or (structural-editing-mcp.tree:get-node-path file-node) (append d-path (list f-idx)))
          for filepath = (structural-editing-mcp.workspace:get-filepath f-path)
          for form-count = (length (structural-editing-mcp.tree:get-node-children file-node))
          do
          (format s "    [~{~A~^, ~}] :FILE (~A) — ~A top-level forms~%"
                  f-path (or filepath "unknown") form-count))
    (when (< end total)
      (format s "    ... (~A more files; use limit and offset in read_node to paginate)~%"
              (- total end)))))

(defun format-workspace-preview
       (s children &key (limit 50) (offset 0))
  "Format preview for workspace root to stream S with pagination."
  (if (null children)
    (format s "Workspace is empty. Provide load_files in read_node to load files into the workspace.~%")
    (progn
      (format s "Active Dialects (~A):~%" (length children))
      (loop for dialect-node in children
            for d-idx from 0
            for d-path = (or (structural-editing-mcp.tree:get-node-path dialect-node) (list d-idx))
            do
            (format-dialect-node-preview s dialect-node d-path :limit limit :offset offset)))))

(defun format-dialect-preview
       (s children path tag &key (limit 50) (offset 0))
  "Format preview for a dialect partition node to stream S with optional pagination."
  (format s "Dialect: ~A~%" tag)
  (if (null children)
    (format s "No files loaded for this dialect.~%")
    (let* ((total (length children))
           (start (min (max 0 (or offset 0)) total))
           (effective-limit (or limit 50))
           (end (min (+ start effective-limit) total))
           (slice (subseq children start end)))
      (if (or (plusp start) (< end total))
        (format s "Files Loaded (~A, showing ~A-~A):~%" total (1+ start) end)
        (format s "Files Loaded (~A):~%" total))
      (loop for file-node in slice
            for idx from start
            for f-path = (or (structural-editing-mcp.tree:get-node-path file-node) (append path (list idx)))
            for filepath = (structural-editing-mcp.workspace:get-filepath f-path)
            for form-count = (length (structural-editing-mcp.tree:get-node-children file-node))
            do
            (format s "  [~{~A~^, ~}] :FILE (~A) — ~A top-level forms~%"
                    f-path (or filepath "unknown") form-count))
      (when (< end total)
        (format s "  ... (~A more files; use limit and offset in read_node to paginate)~%"
                (- total end))))))

(defun format-node-code-and-metrics
       (s node code children skeleton-p)
  "Format either SKELETON metrics or full rendered CODE to stream S."
  (if skeleton-p
    (progn
      (format s "Mode: SKELETON (full code suppressed to prevent context blowout)~%")
      (format s "Metrics:~%")
      (format s "  Direct Child Forms: ~D~%" (length children))
      (format s "  Total Subtree Nodes: ~D~%" (count-subtree-nodes node))
      (format s "  Approx Code Length: ~D characters~%" (length code)))
    (format s "Code:~%~A~%" code)))

(defun format-node-preview
       (node &key (depth 2) (limit 50) (offset 0) (mode "full"))
  "Format a node with its path, tag, rendered code, and summary of children up to DEPTH with pagination. Supports 'skeleton' mode to prevent context blowout on wide nodes."
  (if (null node)
    "Node not found at given path."
    (let* ((path (structural-editing-mcp.tree:get-node-path node))
           (tag (structural-editing-mcp.tree:get-node-tag node))
           (children (structural-editing-mcp.tree:get-node-children node))
           (code (structural-editing-mcp.parser:sexp-to-string node))
           (skeleton-p
             (or (equal mode "skeleton")
                 (and (not (equal mode "full"))
                      (or (>= (length children) *skeleton-threshold-children*)
                          (> (length code) *skeleton-threshold-chars*)))
                 (and (equal mode "full")
                      (or (>= (length children) 200) (> (length code) 20000))))))
      (with-output-to-string (s)
        (format s "Workspace Revision: ~D~%" structural-editing-mcp.workspace:*workspace-revision*)
        (format s "Path: ~A~%" (or path "()"))
        (format s "Tag: ~A~%" tag)
        (cond
          ((eq tag :workspace)
            (format-workspace-preview s children :limit limit :offset offset))
          ((member tag structural-editing-mcp.workspace:*known-dialects*)
            (format-dialect-preview s children path tag :limit limit :offset offset))
          ((eq tag :file)
            (let ((filepath (structural-editing-mcp.workspace:get-filepath path)))
              (when filepath (format s "File: ~A~%" filepath)))
            (format-node-code-and-metrics s node code children skeleton-p)
            (format-children-preview s children path depth " top-level forms"
                                     :limit limit :offset offset :skeleton skeleton-p))
          (t
            (format-node-code-and-metrics s node code children skeleton-p)
            (format-children-preview s children path depth ""
                                     :limit limit :offset offset :skeleton skeleton-p)))))))

(defun format-ancestor-spine-item
       (tree prefix next-idx)
  "Format a single ancestral spine level showing how NEXT-IDX was entered, eliding siblings."
  (let* ((ancestor (structural-editing-mcp.tree:resolve-tree-scope tree prefix))
         (tag (structural-editing-mcp.tree:get-node-tag ancestor))
         (children (structural-editing-mcp.tree:get-node-children ancestor))
         (total (length children)))
    (cond
      ((eq tag :workspace)
        (format nil "Spine [~{~A~^, ~}] :WORKSPACE (~D active dialects; entering dialect [~A])"
                prefix total next-idx))
      ((member tag structural-editing-mcp.workspace:*known-dialects*)
        (format nil "Spine [~{~A~^, ~}] ~A (~D files; entering file [~A])"
                prefix tag total next-idx))
      ((eq tag :file)
        (let ((filepath (structural-editing-mcp.workspace:get-filepath prefix)))
          (format nil "Spine [~{~A~^, ~}] :FILE (~A) — ~D top-level forms; entering form [~A] (~D lateral siblings omitted)"
                  prefix (or filepath "unknown") total next-idx (max 0 (1- total)))))
      (t
        (let ((sig (extract-node-signature ancestor :max-elements 2 :max-length 50)))
          (format nil "Spine [~{~A~^, ~}] ~A: ~A — ~D children; entering child [~A] (~D lateral siblings omitted)"
                  prefix tag sig total next-idx (max 0 (1- total))))))))

(defun format-node-slice (tree path args)
  "Format a vertical spine from workspace root down to PATH, strictly eliding lateral siblings."
  (let* ((target-node (structural-editing-mcp.tree:resolve-tree-scope tree path))
         (depth (or (gethash "depth" args) 2))
         (limit (gethash "limit" args))
         (offset (or (gethash "offset" args) 0))
         (mode (or (gethash "mode" args) "full")))
    (if (null target-node)
      "Target node not found at given path."
      (with-output-to-string (s)
        (format s "=== Ancestral Spine (Vertical Ray down to ~A) ===~%" path)
        (loop for i from 0 below (length path)
              for prefix = (subseq path 0 i)
              for next-idx = (nth i path)
              for spine-line = (format-ancestor-spine-item tree prefix next-idx)
              do (format s "  ~A~%" spine-line))
        (format s "~%=== Target Node [~{~A~^, ~}] ===~%" path)
        (format s "~A"
                (format-node-preview target-node :depth depth :limit limit :offset offset :mode mode))))))

(defun format-workspaces-list (list)
  "Format list of workspace plists into readable string summary."
  (with-output-to-string (s)
    (format s "Workspaces (~A):~%" (length list))
    (dolist (w list)
      (format s "  - [~A] (parent: ~A, revision: ~A, files: ~A, dirty: ~A)~%"
              (getf w :id)
              (or (getf w :parent-id) "none")
              (getf w :revision)
              (getf w :file-count)
              (if (getf w :dirty-p) "YES" "no")))))

(defun format-workspace-status-summary (st)
  "Format workspace status plist ST into a readable summary string."
  (with-output-to-string (s)
    (format s "Workspace: ~A~%" (getf st :id))
    (format s "  Parent: ~A (base revision: ~A)~%"
            (or (getf st :parent-id) "none")
            (getf st :base-revision))
    (format s "  Current Revision: ~A~%" (getf st :revision))
    (let ((dirty (getf st :dirty-files))
          (clean (getf st :clean-files))
          (snaps (getf st :snapshots)))
      (format s "  Dirty Files (~A):~%~{    - ~A~%~}" (length dirty) dirty)
      (format s "  Clean Files (~A):~%~{    - ~A~%~}" (length clean) clean)
      (format s "  Snapshots (~A):~%~{    - ~A~%~}" (length snaps) snaps))))

(defun format-diff-section (s label items &optional prefix)
  "Format a list of ITEMS under LABEL to stream S with optional line PREFIX."
  (when items
    (if prefix
      (format s "  ~A (~A):~%~{    ~A ~A~%~}"
              label (length items)
              (loop for x in items collect prefix collect x))
      (format s "  ~A (~A):~%~{    - ~A~%~}" label (length items) items))))

(defun format-file-diff-summary (s fdiff)
  "Format detailed per-file diff entry FDIFF to stream S."
  (let ((file (getf fdiff :file))
        (status (getf fdiff :status)))
    (format s "    File: ~A (~A)~%" file status)
    (when (getf fdiff :added-forms)
      (format s "      Added forms: ~A~%" (getf fdiff :added-forms)))
    (when (getf fdiff :removed-forms)
      (format s "      Removed forms: ~A~%" (getf fdiff :removed-forms)))
    (when (getf fdiff :modified-forms)
      (format s "      Modified forms (~A):~%" (length (getf fdiff :modified-forms)))
      (dolist (m (getf fdiff :modified-forms))
        (format s "        - Form ~A [~{~A~^, ~}]: ~A~%"
                (getf m :index)
                (or (getf m :path) '())
                (ellipsize (or (getf m :new-snippet) (getf m :old-snippet) "") 40))))))

(defun format-workspace-diff-summary (df)
  "Format workspace diff plist DF into a readable summary string."
  (with-output-to-string (s)
    (format s "Diff between [~A] and [~A]:~%" (getf df :source-id) (getf df :target-id))
    (let ((so (getf df :source-only))
          (to (getf df :target-only))
          (smo (getf df :source-modified-only))
          (tmo (getf df :target-modified-only))
          (ast-merge (getf df :ast-mergeable))
          (both (getf df :modified-in-both))
          (details (getf df :conflict-details))
          (ast-diff (getf df :ast-differing-files)))
      (format-diff-section s "Files only in source" so "+")
      (format-diff-section s "Files only in target" to "-")
      (format-diff-section s "Source modified only" smo "*")
      (format-diff-section s "Target modified only" tmo "*")
      (format-diff-section s "AST-mergeable disjoint files (safe to auto-merge)" ast-merge)
      (when both
        (format s "  COLLIDING MODIFICATIONS in both (~A):~%~{    ! ~A~%~}" (length both) both)
        (when details
          (dolist (d details)
            (format s "      File: ~A (conflicting form indices: ~{~A~^, ~})~%"
                    (getf d :file) (getf d :conflicts)))))
      (format-diff-section s "AST differing files" ast-diff)
      (when (and (null so) (null to) (null smo) (null tmo)
                 (null ast-merge) (null both) (null ast-diff))
        (format s "  No differences detected.~%")))))

(defun format-rebase-conflict-summary (ws-id conflicts)
  "Format a list of rebase collisions into an informative error message."
  (with-output-to-string (s)
    (format s "Rebase of workspace ~S encountered ~A collision(s):~%" ws-id (length conflicts))
    (dolist (c conflicts)
      (format s "  - File: ~A (modified in both)~%" (getf c :file)))))
