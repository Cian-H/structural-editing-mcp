(in-package :structural-editing-mcp-tests)

(deftest test-mcp-initialize
         (testing "handle-message parses initialize request"
                  (let* ((msg (structural-editing-mcp.mcp::dict
                                "jsonrpc" "2.0"
                                "id" 1
                                "method" "initialize"
                                "params" (make-hash-table)))
                         (*standard-output* (make-string-output-stream))
                         (result-str (progn
                                       (structural-editing-mcp.mcp:handle-message msg)
                                       (get-output-stream-string *standard-output*)))
                         (result-json (let ((yason:*parse-json-arrays-as-vectors* nil))
                                        (yason:parse result-str))))
                    (ok (equal (gethash "jsonrpc" result-json) "2.0"))
                    (ok (equal (gethash "id" result-json) 1))
                    (let ((res (gethash "result" result-json)))
                      (ok (equal (gethash "protocolVersion" res) "2024-11-05"))
                      (let ((info (gethash "serverInfo" res)))
                        (ok (equal (gethash "name" info) "structural-editing-mcp")))))))

(deftest test-mcp-tools-list
         (testing "handle-message parses tools/list and exposes 5 unified tools"
                  (let* ((msg (structural-editing-mcp.mcp::dict
                                "jsonrpc" "2.0"
                                "id" 2
                                "method" "tools/list"
                                "params" (make-hash-table)))
                         (*standard-output* (make-string-output-stream))
                         (result-str (progn
                                       (structural-editing-mcp.mcp:handle-message msg)
                                       (get-output-stream-string *standard-output*)))
                         (result-json (let ((yason:*parse-json-arrays-as-vectors* nil))
                                        (yason:parse result-str))))
                    (let* ((res (gethash "result" result-json))
                           (tools (gethash "tools" res)))
                      (ok (listp tools))
                      (ok (= (length tools) 15))
                      (let ((names (mapcar (lambda (x) (gethash "name" x)) tools)))
                        (ok (member "read_node" names :test #'equal))
                        (ok (not (member "read_workspace" names :test #'equal)))
                        (ok (member "ast_modify" names :test #'equal))
                        (ok (member "ast_remove" names :test #'equal))
                        (ok (member "ast_relocate" names :test #'equal))
                        (ok (member "ast_lint" names :test #'equal))
                        (ok (member "ast_complexity_metrics" names :test #'equal))
                        (ok (member "ast_find_duplicates" names :test #'equal))
                        (ok (member "ast_analyze_bindings" names :test #'equal))
                        (ok (member "ast_suggest_refactorings" names :test #'equal))
                        (ok (member "ast_extract_function" names :test #'equal))
                        (ok (member "commit_workspace" names :test #'equal)))))))

(deftest test-mcp-ast-operations
         (testing "unified read_node, ast_modify, ast_remove, ast_relocate in memory"
                  ;; Setup a test workspace
                  (structural-editing-mcp.workspace:init-workspace)
                  (with-open-file (f "/tmp/test.lisp" :direction :output :if-exists :supersede)
                    (write-string "(defun foo () 1) (defun bar () 2)" f))

                  ;; Load file implicitly through read_node
                  (let* ((req (structural-editing-mcp.mcp::dict
                                "jsonrpc" "2.0"
                                "id" 3
                                "method" "tools/call"
                                "params" (structural-editing-mcp.mcp::dict
                                           "name" "read_node"
                                           "arguments" (structural-editing-mcp.mcp::dict
                                                         "path" #()
                                                         "load_files" #("/tmp/test.lisp")))))
                         (*standard-output* (make-string-output-stream))
                         (out-str (progn
                                    (structural-editing-mcp.mcp:handle-message req)
                                    (get-output-stream-string *standard-output*)))
                         (json (let ((yason:*parse-json-arrays-as-vectors* nil))
                                 (yason:parse out-str)))
                         (content (first (gethash "content" (gethash "result" json)))))
                    (ok (search "Tag: WORKSPACE" (gethash "text" content)))
                    (ok (search "Active Dialects (1):" (gethash "text" content)))
                    (ok (search "/tmp/test.lisp" (gethash "text" content))))

                  ;; Test read_node on dialect 0 (path [0])
                  (let* ((read-msg (structural-editing-mcp.mcp::dict
                                     "jsonrpc" "2.0"
                                     "id" 10
                                     "method" "tools/call"
                                     "params" (structural-editing-mcp.mcp::dict
                                                "name" "read_node"
                                                "arguments" (structural-editing-mcp.mcp::dict
                                                              "path" '(0)))))
                         (*standard-output* (make-string-output-stream))
                         (out-str (progn
                                    (structural-editing-mcp.mcp:handle-message read-msg)
                                    (get-output-stream-string *standard-output*)))
                         (json (let ((yason:*parse-json-arrays-as-vectors* nil))
                                 (yason:parse out-str)))
                         (content (first (gethash "content" (gethash "result" json)))))
                    (ok (search "Path: (0)" (gethash "text" content)))
                    (ok (search "Tag: COMMON-LISP" (gethash "text" content)))
                    (ok (search "Files Loaded (1):" (gethash "text" content))))

                  ;; Test read_node on file 0 in dialect 0 (path [0, 0])
                  (let* ((read-file-msg (structural-editing-mcp.mcp::dict
                                          "jsonrpc" "2.0"
                                          "id" 101
                                          "method" "tools/call"
                                          "params" (structural-editing-mcp.mcp::dict
                                                     "name" "read_node"
                                                     "arguments" (structural-editing-mcp.mcp::dict
                                                                   "path" '(0 0)))))
                         (*standard-output* (make-string-output-stream))
                         (out-str (progn
                                    (structural-editing-mcp.mcp:handle-message read-file-msg)
                                    (get-output-stream-string *standard-output*)))
                         (json (let ((yason:*parse-json-arrays-as-vectors* nil))
                                 (yason:parse out-str)))
                         (content (first (gethash "content" (gethash "result" json)))))
                    (ok (search "Path: (0 0)" (gethash "text" content)))
                    (ok (search "Children (2 top-level forms):" (gethash "text" content))))

                  ;; Test ast_modify: insert new function at path [0, 0, 1]
                  (let* ((insert-msg (structural-editing-mcp.mcp::dict
                                       "jsonrpc" "2.0"
                                       "id" 11
                                       "method" "tools/call"
                                       "params" (structural-editing-mcp.mcp::dict
                                                  "name" "ast_modify"
                                                  "arguments" (structural-editing-mcp.mcp::dict
                                                                "path" '(0 0 1)
                                                                "action" "insert"
                                                                "new_node" "(defun baz () 3)"))))
                         (*standard-output* (make-string-output-stream)))
                    (structural-editing-mcp.mcp:handle-message insert-msg)
                    ;; The file now has 3 functions: foo, baz, bar
                    (let ((file-children (structural-editing-mcp.tree:get-node-children
                                           (structural-editing-mcp.tree:get-node-at-path structural-editing-mcp.workspace:*workspace-tree* '(0 0)))))
                      (ok (= (length file-children) 3))))

                  ;; Test ast_modify: wrap [0, 0, 0] in :paren
                  (let* ((wrap-msg (structural-editing-mcp.mcp::dict
                                     "jsonrpc" "2.0"
                                     "id" 12
                                     "method" "tools/call"
                                     "params" (structural-editing-mcp.mcp::dict
                                                "name" "ast_modify"
                                                "arguments" (structural-editing-mcp.mcp::dict
                                                              "path" '(0 0 0)
                                                              "action" "wrap"
                                                              "new_node" ":paren"))))
                         (*standard-output* (make-string-output-stream)))
                    (structural-editing-mcp.mcp:handle-message wrap-msg)
                    (let ((wrapped (structural-editing-mcp.tree:get-node-at-path structural-editing-mcp.workspace:*workspace-tree* '(0 0 0))))
                      (ok (eq (structural-editing-mcp.tree:get-node-tag wrapped) :paren))
                      ;; Verify rendered string does not contain raw :path keywords
                      (let ((code (structural-editing-mcp.parser:sexp-to-string wrapped)))
                        (ok (not (search ":path" code)))
                        (ok (search "((defun foo" code)))))

                  ;; Test ast_remove: unwrap [0, 0, 0]
                  (let* ((unwrap-msg (structural-editing-mcp.mcp::dict
                                       "jsonrpc" "2.0"
                                       "id" 13
                                       "method" "tools/call"
                                       "params" (structural-editing-mcp.mcp::dict
                                                  "name" "ast_remove"
                                                  "arguments" (structural-editing-mcp.mcp::dict
                                                                "path" '(0 0 0)
                                                                "action" "unwrap"))))
                         (*standard-output* (make-string-output-stream)))
                    (structural-editing-mcp.mcp:handle-message unwrap-msg)
                    (let ((node (structural-editing-mcp.tree:get-node-at-path structural-editing-mcp.workspace:*workspace-tree* '(0 0 0))))
                      (ok (search "(defun foo" (structural-editing-mcp.parser:sexp-to-string node)))))

                  ;; Test ast_relocate: swap [0, 0, 0] and [0, 0, 1]
                  (let* ((swap-msg (structural-editing-mcp.mcp::dict
                                     "jsonrpc" "2.0"
                                     "id" 14
                                     "method" "tools/call"
                                     "params" (structural-editing-mcp.mcp::dict
                                                "name" "ast_relocate"
                                                "arguments" (structural-editing-mcp.mcp::dict
                                                              "source_path" '(0 0 0)
                                                              "target_path" '(0 0 1)
                                                              "action" "swap"))))
                         (*standard-output* (make-string-output-stream)))
                    (structural-editing-mcp.mcp:handle-message swap-msg)
                    (let ((first-node (structural-editing-mcp.tree:get-node-at-path structural-editing-mcp.workspace:*workspace-tree* '(0 0 0))))
                      (ok (search "defun baz" (structural-editing-mcp.parser:sexp-to-string first-node)))))

                  ;; Test ast_relocate: split action
                  (let* ((split-msg (structural-editing-mcp.mcp::dict
                                      "jsonrpc" "2.0"
                                      "id" 15
                                      "method" "tools/call"
                                      "params" (structural-editing-mcp.mcp::dict
                                                 "name" "ast_relocate"
                                                 "arguments" (structural-editing-mcp.mcp::dict
                                                               "target_path" '(0 0)
                                                               "action" "split"
                                                               "index" 2))))
                         (*standard-output* (make-string-output-stream)))
                    (structural-editing-mcp.mcp:handle-message split-msg)
                    (let ((file-node (structural-editing-mcp.tree:get-node-at-path structural-editing-mcp.workspace:*workspace-tree* '(0 0))))
                      (ok (= (length (structural-editing-mcp.tree:get-node-children file-node)) 2))))

                  ;; Test ast_modify: range wrap with end_index
                  (let* ((range-wrap-msg (structural-editing-mcp.mcp::dict
                                           "jsonrpc" "2.0"
                                           "id" 16
                                           "method" "tools/call"
                                           "params" (structural-editing-mcp.mcp::dict
                                                      "name" "ast_modify"
                                                      "arguments" (structural-editing-mcp.mcp::dict
                                                                    "path" '(0 0 0)
                                                                    "action" "wrap"
                                                                    "new_node" ":square"
                                                                    "index" 0
                                                                    "end_index" 1))))
                         (*standard-output* (make-string-output-stream)))
                    (structural-editing-mcp.mcp:handle-message range-wrap-msg)
                    (let ((wrapped (structural-editing-mcp.tree:get-node-at-path structural-editing-mcp.workspace:*workspace-tree* '(0 0 0 0))))
                      (ok (eq (structural-editing-mcp.tree:get-node-tag wrapped) :square))))

                  ;; Test ast_extract_function
                  (let* ((ext-msg (structural-editing-mcp.mcp::dict
                                    "jsonrpc" "2.0"
                                    "id" 17
                                    "method" "tools/call"
                                    "params" (structural-editing-mcp.mcp::dict
                                               "name" "ast_extract_function"
                                               "arguments" (structural-editing-mcp.mcp::dict
                                                             "path" '(0 0 0 0 0 2)
                                                             "function_name" "extracted-helper"
                                                             "params" #("x" "y")))))
                         (*standard-output* (make-string-output-stream)))
                    (structural-editing-mcp.mcp:handle-message ext-msg)
                    (let ((code (structural-editing-mcp.parser:sexp-to-string structural-editing-mcp.workspace:*workspace-tree*)))
                      (ok (search "defun extracted-helper" code))))
                  ;; Test ast_lint
                  (let* ((lint-msg (structural-editing-mcp.mcp::dict
                                     "jsonrpc" "2.0"
                                     "id" 18
                                     "method" "tools/call"
                                     "params" (structural-editing-mcp.mcp::dict
                                                "name" "ast_lint"
                                                "arguments" (structural-editing-mcp.mcp::dict))))
                         (*standard-output* (make-string-output-stream))
                         (raw-resp (progn
                                     (structural-editing-mcp.mcp:handle-message lint-msg)
                                     (get-output-stream-string *standard-output*)))
                         (parsed-resp (let ((yason:*parse-json-arrays-as-vectors* nil))
                                        (yason:parse raw-resp))))
                    (let* ((res (gethash "result" parsed-resp))
                           (content (first (gethash "content" res)))
                           (text (gethash "text" content)))
                      (ok (stringp text))
                      (ok (or (search "anti-pattern" text) (search "No anti-patterns" text)))))
                  ;; Test ast_complexity_metrics
                  (let* ((comp-msg (structural-editing-mcp.mcp::dict
                                     "jsonrpc" "2.0"
                                     "id" 19
                                     "method" "tools/call"
                                     "params" (structural-editing-mcp.mcp::dict
                                                "name" "ast_complexity_metrics"
                                                "arguments" (structural-editing-mcp.mcp::dict
                                                              "min_complexity" 1))))
                         (*standard-output* (make-string-output-stream))
                         (raw-resp (progn
                                     (structural-editing-mcp.mcp:handle-message comp-msg)
                                     (get-output-stream-string *standard-output*)))
                         (parsed-resp (let ((yason:*parse-json-arrays-as-vectors* nil))
                                        (yason:parse raw-resp))))
                    (let* ((res (gethash "result" parsed-resp))
                           (content (first (gethash "content" res)))
                           (text (gethash "text" content)))
                      (ok (stringp text))
                      (ok (search "Structural Complexity Report" text))))
                  ;; Test ast_find_duplicates
                  (let* ((dup-msg (structural-editing-mcp.mcp::dict
                                    "jsonrpc" "2.0"
                                    "id" 20
                                    "method" "tools/call"
                                    "params" (structural-editing-mcp.mcp::dict
                                               "name" "ast_find_duplicates"
                                               "arguments" (structural-editing-mcp.mcp::dict
                                                             "min_nodes" 2
                                                             "min_depth" 1))))
                         (*standard-output* (make-string-output-stream))
                         (raw-resp (progn
                                     (structural-editing-mcp.mcp:handle-message dup-msg)
                                     (get-output-stream-string *standard-output*)))
                         (parsed-resp (let ((yason:*parse-json-arrays-as-vectors* nil))
                                        (yason:parse raw-resp))))
                    (let* ((res (gethash "result" parsed-resp))
                           (content (first (gethash "content" res)))
                           (text (gethash "text" content)))
                      (ok (stringp text))
                      (ok (or (search "Duplicate Subtrees Report" text)
                              (search "No duplicate subtrees or structural clones detected" text)))))
                  ;; Test ast_analyze_bindings
                  (let* ((b-msg (structural-editing-mcp.mcp::dict
                                  "jsonrpc" "2.0"
                                  "id" 21
                                  "method" "tools/call"
                                  "params" (structural-editing-mcp.mcp::dict
                                             "name" "ast_analyze_bindings"
                                             "arguments" (structural-editing-mcp.mcp::dict))))
                         (*standard-output* (make-string-output-stream))
                         (raw-resp (progn
                                     (structural-editing-mcp.mcp:handle-message b-msg)
                                     (get-output-stream-string *standard-output*)))
                         (parsed-resp (let ((yason:*parse-json-arrays-as-vectors* nil))
                                        (yason:parse raw-resp))))
                    (let* ((res (gethash "result" parsed-resp))
                           (content (first (gethash "content" res)))
                           (text (gethash "text" content)))
                      (ok (stringp text))
                      (ok (or (search "Variable Scope & Binding Report" text)
                              (search "No unused or shadowed" text)))))
                  ;; Test ast_suggest_refactorings
                  (let* ((sug-msg (structural-editing-mcp.mcp::dict
                                    "jsonrpc" "2.0"
                                    "id" 22
                                    "method" "tools/call"
                                    "params" (structural-editing-mcp.mcp::dict
                                               "name" "ast_suggest_refactorings"
                                               "arguments" (structural-editing-mcp.mcp::dict))))
                         (*standard-output* (make-string-output-stream))
                         (raw-resp (progn
                                     (structural-editing-mcp.mcp:handle-message sug-msg)
                                     (get-output-stream-string *standard-output*)))
                         (parsed-resp (let ((yason:*parse-json-arrays-as-vectors* nil))
                                        (yason:parse raw-resp))))
                    (let* ((res (gethash "result" parsed-resp))
                           (content (first (gethash "content" res)))
                           (text (gethash "text" content)))
                      (ok (stringp text))
                      (ok (or (search "Structural Refactoring Plan" text)
                              (search "No refactoring opportunities detected" text)))))))

(deftest test-mcp-occ-concurrency
  (testing "implicit agent session tracking OCC prevents race conditions and guides agents"
    (structural-editing-mcp.workspace:init-workspace)
    (with-open-file (f "/tmp/occ-test.lisp" :direction :output :if-exists :supersede)
      (write-string "(defun foo () 1) (defun bar () 2)" f))

    ;; 1. Agent A reads node [0, 0]
    (let* ((read-a (structural-editing-mcp.mcp::dict
                     "jsonrpc" "2.0"
                     "id" 1001
                     "method" "tools/call"
                     "params" (structural-editing-mcp.mcp::dict
                                "name" "read_node"
                                "arguments" (structural-editing-mcp.mcp::dict
                                              "path" '(0 0)
                                              "load_files" #("/tmp/occ-test.lisp")
                                              "agent_id" "agent-a"))))
           (*standard-output* (make-string-output-stream))
           (out-a (progn
                    (structural-editing-mcp.mcp:handle-message read-a)
                    (get-output-stream-string *standard-output*)))
           (json-a (let ((yason:*parse-json-arrays-as-vectors* nil))
                     (yason:parse out-a)))
           (content-a (first (gethash "content" (gethash "result" json-a))))
           (text-a (gethash "text" content-a)))
      (ok (search "Workspace Revision: 1" text-a)))

    ;; 2. Agent B also reads node [0, 0]
    (let* ((read-b (structural-editing-mcp.mcp::dict
                     "jsonrpc" "2.0"
                     "id" 1002
                     "method" "tools/call"
                     "params" (structural-editing-mcp.mcp::dict
                                "name" "read_node"
                                "arguments" (structural-editing-mcp.mcp::dict
                                              "path" '(0 0)
                                              "agent_id" "agent-b"))))
           (*standard-output* (make-string-output-stream))
           (out-b (progn
                    (structural-editing-mcp.mcp:handle-message read-b)
                    (get-output-stream-string *standard-output*)))
           (json-b (let ((yason:*parse-json-arrays-as-vectors* nil))
                     (yason:parse out-b)))
           (content-b (first (gethash "content" (gethash "result" json-b))))
           (text-b (gethash "text" content-b)))
      (ok (search "Workspace Revision: 1" text-b)))

    ;; 3. Agent A modifies form at [0, 0, 1]
    (let* ((mod-a (structural-editing-mcp.mcp::dict
                    "jsonrpc" "2.0"
                    "id" 1003
                    "method" "tools/call"
                    "params" (structural-editing-mcp.mcp::dict
                               "name" "ast_modify"
                               "arguments" (structural-editing-mcp.mcp::dict
                                             "path" '(0 0 1)
                                             "action" "insert"
                                             "new_node" "(defun inserted () 42)"
                                             "agent_id" "agent-a"))))
           (*standard-output* (make-string-output-stream))
           (out-mod-a (progn
                        (structural-editing-mcp.mcp:handle-message mod-a)
                        (get-output-stream-string *standard-output*)))
           (json-mod-a (let ((yason:*parse-json-arrays-as-vectors* nil))
                         (yason:parse out-mod-a)))
           (res-a (gethash "result" json-mod-a))
           (text-res-a (gethash "text" (first (gethash "content" res-a)))))
      (ok (search "Workspace Revision: 2" text-res-a))
      (ok (= structural-editing-mcp.workspace:*workspace-revision* 2)))

    ;; 4. Agent B tries to mutate [0, 0, 1] without re-reading -> CONFLICT REJECTION!
    (let* ((mod-b (structural-editing-mcp.mcp::dict
                    "jsonrpc" "2.0"
                    "id" 1004
                    "method" "tools/call"
                    "params" (structural-editing-mcp.mcp::dict
                               "name" "ast_modify"
                               "arguments" (structural-editing-mcp.mcp::dict
                                             "path" '(0 0 1)
                                             "action" "overwrite"
                                             "new_node" "(defun bar () 99)"
                                             "agent_id" "agent-b"))))
           (*standard-output* (make-string-output-stream))
           (out-mod-b (progn
                        (structural-editing-mcp.mcp:handle-message mod-b)
                        (get-output-stream-string *standard-output*)))
           (json-mod-b (let ((yason:*parse-json-arrays-as-vectors* nil))
                         (yason:parse out-mod-b)))
           (res-b (gethash "result" json-mod-b))
           (err-content (first (gethash "content" res-b)))
           (err-text (gethash "text" err-content)))
      (ok (gethash "isError" res-b))
      (ok (search "Conflict: The workspace was modified by another agent since your last read" err-text))
      (ok (search "Current revision is 2 (your view was at revision 1)" err-text))
      (ok (search "Action required: Call read_node on [0, 0]" err-text)))

    ;; 5. Agent B follows action required and calls read_node on [0, 0]
    (let* ((read-b2 (structural-editing-mcp.mcp::dict
                      "jsonrpc" "2.0"
                      "id" 1005
                      "method" "tools/call"
                      "params" (structural-editing-mcp.mcp::dict
                                 "name" "read_node"
                                 "arguments" (structural-editing-mcp.mcp::dict
                                               "path" '(0 0)
                                               "agent_id" "agent-b"))))
           (*standard-output* (make-string-output-stream)))
      (structural-editing-mcp.mcp:handle-message read-b2)
      (ok (= (gethash "agent-b" structural-editing-mcp.workspace:*agent-views*) 2)))

    ;; 6. Agent B now re-submits its edit with updated target path [0, 0, 2] -> SUCCEEDS!
    (let* ((mod-b2 (structural-editing-mcp.mcp::dict
                     "jsonrpc" "2.0"
                     "id" 1006
                     "method" "tools/call"
                     "params" (structural-editing-mcp.mcp::dict
                                "name" "ast_modify"
                                "arguments" (structural-editing-mcp.mcp::dict
                                              "path" '(0 0 2)
                                              "action" "overwrite"
                                              "new_node" "(defun bar () 99)"
                                              "agent_id" "agent-b"))))
           (*standard-output* (make-string-output-stream))
           (out-b2 (progn
                     (structural-editing-mcp.mcp:handle-message mod-b2)
                     (get-output-stream-string *standard-output*)))
           (json-b2 (let ((yason:*parse-json-arrays-as-vectors* nil))
                      (yason:parse out-b2)))
           (res-b2 (gethash "result" json-b2)))
      (ok (null (gethash "isError" res-b2)))
      (ok (= structural-editing-mcp.workspace:*workspace-revision* 3)))))

(deftest test-mcp-worker-pool
  (testing "worker thread pool processes queued tasks safely and cleanly shuts down"
    (structural-editing-mcp.mcp:start-worker-pool 2)
    (let ((counter 0)
          (lock (bt:make-lock "test-lock")))
      (loop repeat 10
            do (structural-editing-mcp.mcp::enqueue-task
                 (lambda ()
                   (bt:with-lock-held (lock)
                     (incf counter)))))
      ;; Allow worker threads to drain queue
      (loop repeat 30
            until (bt:with-lock-held (lock) (= counter 10))
            do (sleep 0.05))
      (ok (= counter 10)))
    (structural-editing-mcp.mcp:stop-worker-pool)))

