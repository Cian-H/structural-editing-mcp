(in-package :structural-editing-mcp-tests)

(deftest test-workspace
  (init-workspace)
  (ok (equal '(:path () :workspace) *workspace-tree*))
  (uiop:with-temporary-file (:pathname p :stream s :direction :output)
    (write-string "(defn add [a b] (+ a b))" s)
    :close-stream
    (let ((id (read-workspace-file p)))
      (ok (= 0 id))
      (ok (equal (namestring p) (namestring (get-filepath id))))
      (ok (= 1 (length (get-node-children *workspace-tree*))))
      (setf *workspace-tree*
            (overwrite-node *workspace-tree* '(0 0 1) (list :path '(0 0 1) :leaf 'sum)))
      (write-workspace)
      (ok (search "sum" (uiop:read-file-string p))))))
