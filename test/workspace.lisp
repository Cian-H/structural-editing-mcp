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

(deftest test-workspace-directory-scanning
  (testing "lisp-file-p recognizes lisp extensions and ignores others"
    (ok (lisp-file-p "foo.lisp"))
    (ok (lisp-file-p "foo.cl"))
    (ok (lisp-file-p "foo.asd"))
    (ok (lisp-file-p "foo.lsp"))
    (ok (lisp-file-p "FOO.LISP"))
    (ok (not (lisp-file-p "license.md")))
    (ok (not (lisp-file-p "devenv.lock")))
    (ok (not (lisp-file-p "server-binary"))))

  (testing "collect-lisp-files scans directories recursively and ignores non-lisp"
    (let ((files (collect-lisp-files "src/")))
      (ok (> (length files) 0))
      (ok (every #'lisp-file-p files))
      (ok (find (namestring (truename "src/parser.lisp")) files :test #'string=))))

  (testing "load-into-workspace loads directory and deduplicates"
    (init-workspace)
    (let ((ids1 (load-into-workspace "src/")))
      (ok (> (length ids1) 0))
      (let ((count1 (length (get-node-children *workspace-tree*))))
        (ok (= count1 (length ids1)))
        ;; Calling again should deduplicate and not add extra files
        (let ((ids2 (load-into-workspace "src/")))
          (ok (= (length (get-node-children *workspace-tree*)) count1)))))))
