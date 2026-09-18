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
        (ok (= (length tools) 10))
        (let ((names (mapcar (lambda (x) (gethash "name" x)) tools)))
          (ok (member "read_node" names :test #'equal))
          (ok (not (member "read_workspace" names :test #'equal)))
          (ok (member "ast_modify" names :test #'equal))
          (ok (member "ast_remove" names :test #'equal))
          (ok (member "ast_relocate" names :test #'equal))
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
          (ok (search "defun extracted-helper" code))))))


