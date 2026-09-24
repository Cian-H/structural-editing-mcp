(in-package :structural-editing-mcp-tests)

(deftest test-workspace
         (init-workspace)
         (ok (equal '(:path () :workspace) *workspace-tree*))
         (uiop:with-temporary-file (:pathname p :stream s :direction :output :type "lisp")
                                   (write-string "(defn add [a b] (+ a b))" s)
                                   :close-stream
                                   (let ((id (read-workspace-file p)))
                                     (ok (= 0 id))
                                     (ok (equal (namestring p) (namestring (get-filepath id))))
                                     (ok (= 1 (length (get-node-children *workspace-tree*))))
                                     (let ((dialect-node (first (get-node-children *workspace-tree*))))
                                       (ok (eq :common-lisp (get-node-tag dialect-node)))
                                       (ok (= 1 (length (get-node-children dialect-node)))))
                                     (setf *workspace-tree*
                                           (overwrite-node *workspace-tree* '(0 0 0 1) (list :path '(0 0 0 1) :leaf 'sum)))
                                     (write-workspace)
                                     (ok (search "sum" (uiop:read-file-string p))))))

(deftest test-workspace-directory-scanning
         (testing "lisp-file-p recognizes all supported dialect extensions and ignores others"
                  (ok (lisp-file-p "foo.lisp"))
                  (ok (lisp-file-p "foo.cl"))
                  (ok (lisp-file-p "foo.asd"))
                  (ok (lisp-file-p "foo.lsp"))
                  (ok (lisp-file-p "FOO.LISP"))
                  (ok (lisp-file-p "foo.clj"))
                  (ok (lisp-file-p "foo.cljs"))
                  (ok (lisp-file-p "foo.cljc"))
                  (ok (lisp-file-p "foo.edn"))
                  (ok (lisp-file-p "foo.scm"))
                  (ok (lisp-file-p "foo.ss"))
                  (ok (lisp-file-p "foo.rkt"))
                  (ok (lisp-file-p "foo.sld"))
                  (ok (lisp-file-p "foo.el"))
                  (ok (lisp-file-p "foo.fnl"))
                  (ok (not (lisp-file-p "license.md")))
                  (ok (not (lisp-file-p "devenv.lock")))
                  (ok (not (lisp-file-p "server-binary"))))

         (testing "file-dialect classifies dialects correctly"
                  (ok (eq :common-lisp (file-dialect "test.lisp")))
                  (ok (eq :common-lisp (file-dialect "test.asd")))
                  (ok (eq :clojure (file-dialect "core.clj")))
                  (ok (eq :clojure (file-dialect "app.cljs")))
                  (ok (eq :scheme (file-dialect "sicp.scm")))
                  (ok (eq :scheme (file-dialect "macro.rkt")))
                  (ok (eq :emacs-lisp (file-dialect "init.el")))
                  (ok (eq :fennel (file-dialect "game.fnl")))
                  (ok (null (file-dialect "readme.txt"))))

         (testing "collect-lisp-files scans directories recursively and ignores non-lisp"
                  (let ((files (collect-lisp-files "src/")))
                    (ok (> (length files) 0))
                    (ok (every #'lisp-file-p files))
                    (ok (find (namestring (truename "src/parser.lisp")) files :test #'string=))))

         (testing "load-into-workspace loads directory and deduplicates"
                  (init-workspace)
                  (let ((ids1 (load-into-workspace "src/")))
                    (ok (> (length ids1) 0))
                    (let* ((dialect-node (first (get-node-children *workspace-tree*)))
                           (count1 (length (get-node-children dialect-node))))
                      (ok (eq :common-lisp (get-node-tag dialect-node)))
                      (ok (= count1 (length ids1)))
                      ;; Calling again should deduplicate and not add extra files
                      (let ((ids2 (load-into-workspace "src/")))
                        (let ((dialect-node-after (first (get-node-children *workspace-tree*))))
                          (ok (= (length (get-node-children dialect-node-after)) count1))))))))

(deftest test-multi-dialect-workspace
         (testing "multiple dialects are partitioned into separate sub-trees"
                  (init-workspace)
                  (uiop:with-temporary-file (:pathname p-cl :stream s-cl :direction :output :type "lisp")
                                            (write-string "(defun cl-func () :common-lisp)" s-cl)
                                            :close-stream
                                            (uiop:with-temporary-file (:pathname p-clj :stream s-clj :direction :output :type "clj")
                                                                      (write-string "(defn clj-func [x] (str \"clojure: \" x))" s-clj)
                                                                      :close-stream
                                                                      (uiop:with-temporary-file (:pathname p-scm :stream s-scm :direction :output :type "scm")
                                                                                                (write-string "(define (scm-func x) #t)" s-scm)
                                                                                                :close-stream
                                                                                                (let ((id-cl (read-workspace-file p-cl))
                                                                                                      (id-clj (read-workspace-file p-clj))
                                                                                                      (id-scm (read-workspace-file p-scm)))
                                                                                                  (ok (= 0 id-cl))
                                                                                                  (ok (= 1 id-clj))
                                                                                                  (ok (= 2 id-scm))
                                                                                                  ;; The workspace tree has 3 dialect children
                                                                                                  (let ((dialect-nodes (get-node-children *workspace-tree*)))
                                                                                                    (ok (= 3 (length dialect-nodes)))
                                                                                                    (ok (eq :common-lisp (get-node-tag (first dialect-nodes))))
                                                                                                    (ok (eq :clojure (get-node-tag (second dialect-nodes))))
                                                                                                    (ok (eq :scheme (get-node-tag (third dialect-nodes)))))
                                                                                                  ;; File nodes are under their respective dialect nodes
                                                                                                  (let ((cl-file (get-node-at-path *workspace-tree* '(0 0)))
                                                                                                        (clj-file (get-node-at-path *workspace-tree* '(1 0)))
                                                                                                        (scm-file (get-node-at-path *workspace-tree* '(2 0))))
                                                                                                    (ok (eq :file (get-node-tag cl-file)))
                                                                                                    (ok (eq :file (get-node-tag clj-file)))
                                                                                                    (ok (eq :file (get-node-tag scm-file)))
                                                                                                    (ok (equal (namestring p-cl) (namestring (get-filepath '(0 0)))))
                                                                                                    (ok (equal (namestring p-clj) (namestring (get-filepath '(1 0)))))
                                                                                                    (ok (equal (namestring p-scm) (namestring (get-filepath '(2 0)))))
                                                                                                    ;; Mutate Clojure form and persist across dialects
                                                                                                    (setf *workspace-tree*
                                                                                                          (overwrite-node *workspace-tree* '(1 0 0 1) (list :path '(1 0 0 1) :leaf 'clj-updated)))
                                                                                                    (write-workspace)
                                                                                                    (ok (search "clj-updated" (uiop:read-file-string p-clj))))))))))

(deftest test-workspace-precision-persistence
         (testing "unmodified files in multi-file workspace are untouched on disk"
                  (init-workspace)
                  (uiop:with-temporary-file (:pathname p-mod :stream s-mod :direction :output :type "lisp")
                                            (write-string "(defpackage :mod-pkg (:use :cl))

(defun fn-one (x)
  \"Documentation with    spaces.\"
  (+ x 1))

(defun fn-two (y)
  (* y 2))
" s-mod)
                                            :close-stream
                                            (uiop:with-temporary-file (:pathname p-clean :stream s-clean :direction :output :type "lisp")
                                                                      (write-string "(defpackage :clean-pkg (:use :cl))

(defun clean-fn (z)
  (- z 100))
" s-clean)
                                                                      :close-stream
                                                                      (let ((id-mod (read-workspace-file p-mod))
                                                                            (id-clean (read-workspace-file p-clean)))
                                                                        (ok (= 0 id-mod))
                                                                        (ok (= 1 id-clean))
                                                                        (let ((file-mod-node (get-node-at-path *workspace-tree* '(0 0)))
                                                                              (file-clean-node (get-node-at-path *workspace-tree* '(0 1))))
                                                                          (ok (file-clean-p file-mod-node))
                                                                          (ok (file-clean-p file-clean-node))
                                                                          ;; Mutate only fn-two in p-mod: (0 0 2 1)
                                                                          (setf *workspace-tree*
                                                                                (overwrite-node *workspace-tree* '(0 0 2 1) (list :path '(0 0 2 1) :leaf 'fn-two-renamed)))
                                                                          (let ((file-mod-after (get-node-at-path *workspace-tree* '(0 0)))
                                                                                (file-clean-after (get-node-at-path *workspace-tree* '(0 1)))
                                                                                (clean-mtime-before (file-write-date p-clean)))
                                                                            (ok (not (file-clean-p file-mod-after)))
                                                                            (ok (file-clean-p file-clean-after))
                                                                            ;; Write workspace: clean file must not be rewritten!
                                                                            (write-workspace)
                                                                            (let ((clean-mtime-after (file-write-date p-clean))
                                                                                  (mod-content (uiop:read-file-string p-mod)))
                                                                              (ok (= clean-mtime-before clean-mtime-after))
                                                                              ;; Modified file has renamed fn-two
                                                                              (ok (search "fn-two-renamed" mod-content))
                                                                              ;; Unmodified fn-one retains exact byte-for-byte formatting and docstring
                                                                              (ok (search "\"Documentation with    spaces.\"" mod-content))
                                                                              (ok (search "(defpackage :mod-pkg (:use :cl))" mod-content))))))))))

(deftest test-workspace-isolated-contexts
  (testing "with-workspace-context provides isolated multi-workspace sessions"
    (let ((ctx-a (make-workspace-context))
          (ctx-b (make-workspace-context)))
      ;; Context A: initialize and add a CL form
      (with-workspace-context (ctx-a)
        (init-workspace)
        (setf *workspace-tree* '(:path () :workspace
                                 (:path (0) :common-lisp
                                  (:path (0 0) :file
                                   (:path (0 0 0) :leaf a)))))
        (ok (= 1 (length (get-node-children *workspace-tree*))))
        (ok (equal '(:path (0 0 0) :leaf a) (get-node-at-path *workspace-tree* '(0 0 0)))))

      ;; Context B: should be independent, empty workspace
      (with-workspace-context (ctx-b)
        (init-workspace)
        (ok (equal '(:path () :workspace) *workspace-tree*))
        (setf *workspace-tree* '(:path () :workspace
                                 (:path (0) :clojure
                                  (:path (0 0) :file
                                   (:path (0 0 0) :leaf b)))))
        (ok (eq :clojure (get-node-tag (first (get-node-children *workspace-tree*))))))

      ;; Back in Context A: verify state was preserved and isolated
      (with-workspace-context (ctx-a)
        (ok (eq :common-lisp (get-node-tag (first (get-node-children *workspace-tree*)))))
        (ok (equal '(:path (0 0 0) :leaf a) (get-node-at-path *workspace-tree* '(0 0 0))))))))

(deftest test-workspace-helpers
  (testing "normalize-agent-id handles nil, empty string, and custom id"
    (ok (equal (normalize-agent-id nil) "default"))
    (ok (equal (normalize-agent-id "") "default"))
    (ok (equal (normalize-agent-id "agent-42") "agent-42")))
  (testing "safe-truename returns valid namestring or nil"
    (ok (stringp (safe-truename "src/workspace.lisp")))
    (ok (null (safe-truename "non-existent-path-abc-123.xyz"))))
  (testing "ensure-workspace initializes if tree is nil"
    (setf *workspace-tree* nil)
    (ensure-workspace)
    (ok (not (null *workspace-tree*)))))

(deftest test-workspace-registry
  (testing "workspace creation, retrieval, and listing"
    (let ((ws1 (create-workspace "test-ws-1"))
          (ws2 (create-workspace "test-ws-2" :parent-id "test-ws-1" :base-revision 5)))
      (ok (equal "test-ws-1" (workspace-context-id ws1)))
      (ok (equal "test-ws-2" (workspace-context-id ws2)))
      (ok (equal "test-ws-1" (workspace-context-parent-id ws2)))
      (ok (= 5 (workspace-context-base-revision ws2)))
      (ok (eq ws1 (get-workspace "test-ws-1")))
      (ok (eq ws2 (get-workspace "test-ws-2")))
      (let ((listed (list-workspaces)))
        (ok (find "test-ws-1" listed :key (lambda (p) (getf p :id)) :test #'equal))
        (ok (find "test-ws-2" listed :key (lambda (p) (getf p :id)) :test #'equal)))
      ;; Duplicate creation error
      (ok (signals (create-workspace "test-ws-1") 'workspace-error))
      ;; Deletion
      (delete-workspace "test-ws-1")
      (delete-workspace "test-ws-2")
      (ok (signals (get-workspace "test-ws-1") 'workspace-not-found-error))
      ;; Deleting default workspace is forbidden
      (ok (signals (delete-workspace "default") 'workspace-error))))

  (testing "copy-workspace-context creates independent deep copy"
    (let* ((orig (create-workspace "orig-ws"))
           (copy (copy-workspace-context orig :new-id "copy-ws")))
      (ok (equal "copy-ws" (workspace-context-id copy)))
      (ok (equal "orig-ws" (workspace-context-parent-id copy)))
      ;; Mutating copy tree does not mutate orig tree
      (setf (workspace-context-tree copy) '(:path () :workspace (:path (0) :leaf mutated)))
      (ok (not (equal (workspace-context-tree orig) (workspace-context-tree copy))))
      (delete-workspace "orig-ws"))))

(deftest test-workspace-lifecycle
  (testing "fork, snapshot, restore, and clear"
    (let* ((ws (create-workspace "life-ws"))
           (cl-node '(:path () :workspace (:path (0) :common-lisp (:path (0 0) :file (:path (0 0 0) :leaf initial))))))
      (setf (workspace-context-tree ws) cl-node)
      ;; Snapshot
      (snapshot-workspace "snap1" ws)
      ;; Mutate tree
      (setf (workspace-context-tree ws) '(:path () :workspace (:path (0) :common-lisp (:path (0 0) :file (:path (0 0 0) :leaf modified)))))
      (ok (not (equal cl-node (workspace-context-tree ws))))
      ;; Restore
      (restore-workspace "snap1" ws)
      (ok (equal cl-node (workspace-context-tree ws)))
      ;; Fork
      (let ((forked (fork-workspace "life-ws" "forked-life-ws")))
        (ok (equal "forked-life-ws" (workspace-context-id forked)))
        (ok (equal "life-ws" (workspace-context-parent-id forked)))
        (ok (equal cl-node (workspace-context-tree forked)))
        (delete-workspace "forked-life-ws"))
      ;; Clear
      (clear-workspace ws)
      (ok (equal '(:path () :workspace) (workspace-context-tree ws)))
      (delete-workspace "life-ws"))))
