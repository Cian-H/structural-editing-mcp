(in-package :structural-editing-mcp-tests/cli)

(deftest test-cli-path-parsing
  (testing "Integer list paths in various formats"
    (ok (null (parse-cli-path nil)))
    (ok (null (parse-cli-path "")))
    (ok (null (parse-cli-path "[]")))
    (ok (equal (parse-cli-path "0,0,2") '(0 0 2)))
    (ok (equal (parse-cli-path "[0, 1, 3]") '(0 1 3)))
    (ok (equal (parse-cli-path "0.2.1") '(0 2 1))))
  (testing "Node handles and symbolic paths"
    (ok (equal (parse-cli-path "#n42") "#n42"))
    (ok (equal (parse-cli-path "#n42:a3f9") "#n42:a3f9"))
    (ok
      (equal (parse-cli-path "main.lisp/defun:main/2") '("main.lisp" "defun:main" 2)))))

(deftest test-cli-arg-parsing
  (testing
      "Commands, positionals, short/long flags, and --"
    (multiple-value-bind (cmd pos flags)
                         (parse-cli-args
                           '("read" "src/main.lisp"
                             "-d"
                             "3"
                             "--mode=skeleton"
                             "--json"
                             "-L"
                             "a.lisp"
                             "-L"
                             "b.lisp"))
      (ok (string= cmd "read"))
      (ok (equal pos '("src/main.lisp")))
      (ok (equal (gethash "depth" flags) "3"))
      (ok (equal (gethash "mode" flags) "skeleton"))
      (ok (eq (gethash "json" flags) t))
      (ok (equal (gethash "load" flags) '("a.lisp" "b.lisp"))))
    (multiple-value-bind (cmd pos flags)
                         (parse-cli-args '("modify" "overwrite" "0,0,0" "--" "-1"))
      (declare (ignore flags))
      (ok (string= cmd "modify"))
      (ok (equal pos '("overwrite" "0,0,0" "-1"))))))

(deftest test-cli-help-version-tools-completions
  (testing
      "Help, version, tools, and completions commands"
    (multiple-value-bind (out code)
                         (execute-cli '("--version"))
      (ok (zerop code))
      (ok (search "structural-editing-mcp" out)))
    (multiple-value-bind (out code)
                         (execute-cli '("--help"))
      (ok (zerop code))
      (ok (search "semcp" out))
      (ok (search "init" out)))
    (multiple-value-bind (out code)
                         (execute-cli '("tools" "--json"))
      (ok (zerop code))
      (let*
          ((yason:*parse-json-arrays-as-vectors* nil) (parsed (yason:parse out)))
        (ok (gethash "ok" parsed))
        (ok (>= (length (gethash "tools" parsed)) 22))))
    (multiple-value-bind (out code)
                         (execute-cli '("completions" "bash"))
      (ok (zerop code))
      (ok (search "_semcp_completions" out)))))

(deftest
    test-cli-oneshot-read-search-lint-and-modify
  (let*
      ((tmp-dir
         (merge-pathnames
           (format nil "semcp-cli-oneshot-~A/" (get-universal-time))
           (truename (uiop:temporary-directory))))
       (test-file (namestring (merge-pathnames "sample.lisp" tmp-dir))))
    (ensure-directories-exist test-file)
    (with-open-file
        (out test-file :direction :output :if-exists :supersede)
      (write-string "(defun greet (x) (if x (progn (print x))))" out))
    (unwind-protect
        (progn
          (testing
              "One-shot read, search, lint, and --fail-on"
            (multiple-value-bind (out code)
                                 (execute-cli (list "read" test-file "--no-session"))
              (ok (zerop code))
              (ok (search "greet" out)))
            (multiple-value-bind (out code)
                                 (execute-cli (list "search" "greet" test-file "--no-session"))
              (ok (zerop code))
              (ok (search "Found 1 matches" out)))
            (multiple-value-bind (out code)
                                 (execute-cli (list "lint" test-file "--no-session" "--fail-on" "any"))
              (ok (= code 2))
              (ok (search "if-progn-to-when" out))))
          (testing
              "One-shot modify with --commit writes to disk"
            (multiple-value-bind (out code)
                                 (execute-cli
                                   (list "modify"
                                         "overwrite"
                                         "0,0,0,1"
                                         "hello"
                                         "-L"
                                         test-file
                                         "--no-session"
                                         "-i"
                                         "--json"))
              (ok (zerop code))
              (let*
                  ((yason:*parse-json-arrays-as-vectors* nil) (parsed (yason:parse out)))
                (ok (gethash "ok" parsed)))
              (let
                  ((disk-content (uiop:read-file-string test-file)))
                (ok (search "hello" disk-content))))))
      (uiop:delete-directory-tree tmp-dir :validate t :if-does-not-exist :ignore))))

(deftest test-cli-in-ram-daemon
  (let*
      ((tmp-dir
         (merge-pathnames
           (format nil "semcp-cli-daemon-~A/" (get-universal-time))
           (truename (uiop:temporary-directory))))
       (session-dir (namestring (merge-pathnames ".semcp/" tmp-dir)))
       (test-file (namestring (merge-pathnames "daemon-app.lisp" tmp-dir)))
       (ready nil))
    (ensure-directories-exist test-file)
    (with-open-file
        (out test-file :direction :output :if-exists :supersede)
      (write-string "(defun ping () :pong)" out))
    (unwind-protect
        (progn
          (multiple-value-bind (out code)
                               (execute-cli (list "init" test-file "--session" session-dir))
            (ok (zerop code))
            (ok (search "1 file loaded" out)))
          (let
              ((daemon-thread
                 (bt:make-thread
                   (lambda ()
                     (run-daemon-foreground session-dir
                                            :ready-hook
                                            (lambda ()
                                              (setf ready t))))
                   :name
                   "test-semcp-daemon")))
            (loop repeat 40 until ready do (sleep 0.02))
            (ok ready)
            (testing
                "Daemon status, in-RAM fork, modify, merge, commit, and stop"
              (multiple-value-bind (out code)
                                   (execute-cli (list "daemon" "status" "--session" session-dir))
                (ok (zerop code))
                (ok (search "Daemon running" out)))
              (multiple-value-bind (out code)
                                   (execute-cli (list "ws" "fork" "ram-feat" "--session" session-dir))
                (ok (zerop code))
                (ok (search "ram-feat" out)))
              (multiple-value-bind (out code)
                                   (execute-cli
                                     (list "modify"
                                           "overwrite"
                                           "0,0,0,1"
                                           "pong"
                                           "-w"
                                           "ram-feat"
                                           "--session"
                                           session-dir))
                (ok (zerop code))
                (ok (search "pong" out)))
              (multiple-value-bind (out code)
                                   (execute-cli
                                     (list "merge" "ram-feat" "--into" "default" "--session" session-dir))
                (ok (zerop code))
                (ok (search "Successfully merged" out)))
              (multiple-value-bind (out code)
                                   (execute-cli (list "commit" "--session" session-dir))
                (ok (zerop code))
                (ok (search "committed" out)))
              (ok (search "pong" (uiop:read-file-string test-file)))
              (multiple-value-bind (out code)
                                   (execute-cli (list "daemon" "stop" "--session" session-dir))
                (ok (zerop code))
                (ok (search "Stopped" out))))
            (bt:join-thread daemon-thread)
            (ok (null (probe-file (daemon-socket-path session-dir))))))
      (uiop:delete-directory-tree tmp-dir :validate t :if-does-not-exist :ignore))))

(deftest
    test-cli-persistent-session-and-branching
  (let*
      ((tmp-dir
         (merge-pathnames
           (format nil "semcp-cli-session-~A/" (get-universal-time))
           (truename (uiop:temporary-directory))))
       (session-dir (namestring (merge-pathnames ".semcp/" tmp-dir)))
       (test-file (namestring (merge-pathnames "app.lisp" tmp-dir))))
    (ensure-directories-exist test-file)
    (with-open-file
        (out test-file :direction :output :if-exists :supersede)
      (write-string
        "(defun alpha () 1)
(defun beta () 2)"
        out))
    (unwind-protect
        (progn
          (testing
              "Init session, fork branch, modify, diff, merge, and commit"
            (multiple-value-bind (out code)
                                 (execute-cli (list "init" test-file "--session" session-dir))
              (ok (zerop code))
              (ok (search "1 file loaded" out)))
            (multiple-value-bind (out code)
                                 (execute-cli (list "ws" "fork" "feat-1" "--session" session-dir))
              (ok (zerop code))
              (ok (search "feat-1" out)))
            (multiple-value-bind (out code)
                                 (execute-cli
                                   (list "modify"
                                         "overwrite"
                                         "0,0,0,1"
                                         "alpha-v2"
                                         "-w"
                                         "feat-1"
                                         "--session"
                                         session-dir))
              (ok (zerop code))
              (ok (search "alpha-v2" out)))
            ;; Default workspace should still be untouched on disk and in default
            (ok (null (search "alpha-v2" (uiop:read-file-string test-file))))
            (multiple-value-bind (out code)
                                 (execute-cli (list "diff" "feat-1" "default" "--session" session-dir))
              (ok (zerop code))
              (ok (search "app.lisp" out)))
            (multiple-value-bind (out code)
                                 (execute-cli (list "merge" "feat-1" "--into" "default" "--session" session-dir))
              (ok (zerop code))
              (ok (search "Successfully merged" out)))
            (multiple-value-bind (out code)
                                 (execute-cli (list "commit" "--session" session-dir))
              (ok (zerop code))
              (ok (search "committed" out)))
            (ok (search "alpha-v2" (uiop:read-file-string test-file))))
          (testing "Raw MCP call subcommand"
            (multiple-value-bind (out code)
                                 (execute-cli
                                   (list "call" "workspace_status" "{}" "--session" session-dir "--json"))
              (ok (zerop code))
              (let*
                  ((yason:*parse-json-arrays-as-vectors* nil) (parsed (yason:parse out)))
                (ok (gethash "ok" parsed))))))
      (uiop:delete-directory-tree tmp-dir :validate t :if-does-not-exist :ignore))))