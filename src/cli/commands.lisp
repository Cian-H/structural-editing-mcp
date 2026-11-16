(in-package :structural-editing-mcp.cli)

(declaim (optimize (speed 2) (safety 3)))

(defun run-mcp-tool
       (tool-name args &key (ws-id "default") (agent-id "cli"))
  "Execute MCP tool TOOL-NAME with hash-table ARGS in workspace WS-ID."
  (let*
      ((ws (get-workspace ws-id))
       (raw-path (gethash "path" args))
       (dialect-arg (gethash "dialect" args))
       (dialect (when dialect-arg (intern (string-upcase dialect-arg) :keyword))))
    (unless (gethash "workspace_id" args)
      (setf (gethash "workspace_id" args) ws-id))
    (unless (gethash "agent_id" args) (setf (gethash "agent_id" args) agent-id))
    (with-workspace-context (ws)
      (execute-tool-call tool-name raw-path args dialect agent-id))))

(defun ensure-loaded-paths
       (paths ws-id &key default-cwd-p)
  "Load PATHS (or CWD if DEFAULT-CWD-P and workspace is empty) into WS-ID."
  (let ((ws (get-workspace ws-id)))
    (with-workspace-context (ws)
      (cond
        (paths (load-into-workspace paths))
        ((and default-cwd-p (null (workspace-all-files-list ws)))
          (load-into-workspace (list (namestring (uiop:getcwd)))))))))

(defun maybe-auto-commit (flags ws-id)
  "If --commit / -i is set and --dry-run is not set, commit dirty files to disk."
  (when
      (and (flag-bool flags "commit" "in-place") (not (flag-bool flags "dry-run")))
    (run-mcp-tool "commit_workspace" (dict) :ws-id ws-id)))

(defun set-common-mutation-flags (args flags)
  "Populate expected_head, expected_revision, and force from FLAGS into ARGS."
  (when-let ((eh (flag-get flags "expected-head")))
    (setf (gethash "expected_head" args) eh))
  (when-let
      ((er (flag-int flags "expected-revision")))
    (setf (gethash "expected_revision" args) er))
  (when (flag-bool flags "force") (setf (gethash "force" args) t))
  args)

(defun set-common-analysis-flags (args flags)
  "Populate path and dialect from FLAGS into ARGS."
  (when-let
      ((p (parse-cli-path (flag-get flags "path"))))
    (setf (gethash "path" args) p))
  (when-let ((d (flag-get flags "dialect"))) (setf (gethash "dialect" args) d))
  args)

(defun resolve-code-input (positional flags)
  "Resolve code/content from --code, --new-node, --content, --stdin, or POSITIONAL."
  (cond
    ((flag-bool flags "stdin") (read-stdin-string))
    ((flag-get flags "code"))
    ((flag-get flags "new-node"))
    ((flag-get flags "content"))
    (t positional)))

(defun target-looks-like-disk-path-p (target)
  "Return T if TARGET is a string that exists on disk as a file or directory."
  (and (stringp target)
       (plusp (length target))
       (not (char= (char target 0) #\#))
       (not (char= (char target 0) #\[))
       (probe-file target)))

(defun cmd-init
       (positionals flags ws-id session-dir)
  "Initialize a persistent .semcp/ workspace session."
  (declare (ignore ws-id))
  (let*
      ((dir
         (uiop:ensure-directory-pathname
           (or (flag-get flags "session")
               (flag-get flags "state-dir")
               (merge-pathnames (concatenate 'string *session-dir-name* "/") (uiop:getcwd)))))
       (load-list
         (or (flag-string-list flags "load")
             positionals
             (unless (flag-bool flags "empty") (list (namestring (uiop:getcwd)))))))
    (declare (ignore session-dir))
    (init-workspace)
    (when load-list (load-into-workspace load-list))
    (let
        ((state-file (save-session dir))
         (count (length (workspace-all-files-list (get-workspace "default")))))
      (values
        (fmt
          "Initialized semcp session in ~A (~D file~:P loaded)."
          (namestring dir)
          count)
        0
        (dict "session_dir"
              (namestring dir)
              "state_file"
              (namestring state-file)
              "files_loaded"
              count)))))

(defun cmd-load (positionals flags ws-id)
  "Load files or directories into the workspace."
  (let
      ((paths (or positionals (flag-string-list flags "load"))))
    (unless paths (error "Usage: semcp load <file-or-dir>..."))
    (ensure-loaded-paths paths ws-id)
    (let
        ((count (length (workspace-all-files-list (get-workspace ws-id)))))
      (values
        (fmt
          "Loaded ~D path(s) into workspace ~S (~D total tracked file~:P)."
          (length paths)
          ws-id
          count)
        0
        nil))))

(defun cmd-read (positionals flags ws-id)
  "Mirror and extend read_node."
  (let*
      ((target (first positionals))
       (explicit-load (flag-string-list flags "load"))
       (args
         (dict "depth"
               (flag-int flags "depth" 2)
               "limit"
               (flag-int flags "limit" 50)
               "offset"
               (flag-int flags "offset" 0)
               "mode"
               (flag-get flags "mode" "auto"))))
    (cond
      ((and target (target-looks-like-disk-path-p target))
        (ensure-loaded-paths (append explicit-load (list target)) ws-id)
        (let ((tru (namestring (truename target))))
          (if (uiop:directory-exists-p target)
            (when-let
                ((p (parse-cli-path (flag-get flags "path"))))
              (setf (gethash "path" args) p))
            (setf (gethash "path" args) (list tru)))))
      (t
        (ensure-loaded-paths explicit-load ws-id :default-cwd-p t)
        (when-let
            ((p (parse-cli-path (or (flag-get flags "path") target))))
          (setf (gethash "path" args) p))))
    (values (run-mcp-tool "read_node" args :ws-id ws-id) 0 nil)))

(defun cmd-slice (positionals flags ws-id)
  "Mirror read_slice."
  (let*
      ((target (or (flag-get flags "path") (first positionals)))
       (parsed (parse-cli-path target)))
    (unless parsed (error "Usage: semcp slice <path> [options]"))
    (ensure-loaded-paths (flag-string-list flags "load") ws-id :default-cwd-p t)
    (let
        ((args
           (dict "path"
                 parsed
                 "depth"
                 (flag-int flags "depth" 2)
                 "limit"
                 (flag-int flags "limit" 50)
                 "offset"
                 (flag-int flags "offset" 0)
                 "mode"
                 (flag-get flags "mode" "full"))))
      (values (run-mcp-tool "read_slice" args :ws-id ws-id) 0 nil))))

(defun cmd-modify (positionals flags ws-id)
  "Mirror ast_modify."
  (let*
      ((action (or (flag-get flags "action") (first positionals)))
       (path-str (or (flag-get flags "path") (second positionals)))
       (code (resolve-code-input (third positionals) flags))
       (parsed-path (parse-cli-path path-str)))
    (unless (and action parsed-path code)
      (error "Usage: semcp modify <insert|overwrite|wrap> <path> <code|--stdin>"))
    (ensure-loaded-paths (flag-string-list flags "load") ws-id)
    (let
        ((args (dict "action" action "path" parsed-path "new_node" code)))
      (when-let ((idx (flag-int flags "index"))) (setf (gethash "index" args) idx))
      (when-let
          ((end-idx (flag-int flags "end-index")))
        (setf (gethash "end_index" args) end-idx))
      (set-common-mutation-flags args flags)
      (let
          ((out (run-mcp-tool "ast_modify" args :ws-id ws-id)))
        (maybe-auto-commit flags ws-id)
        (values out 0 nil)))))

(defun cmd-remove
       (action-override positionals flags ws-id)
  "Mirror ast_remove (also handles delete/unwrap/promote shortcuts)."
  (let*
      ((action (or action-override (flag-get flags "action") (first positionals)))
       (path-str
         (if action-override
           (or (flag-get flags "path") (first positionals))
           (or (flag-get flags "path") (second positionals))))
       (parsed-path (parse-cli-path path-str)))
    (unless (and action parsed-path)
      (error "Usage: semcp remove <delete|unwrap|promote> <path>"))
    (ensure-loaded-paths (flag-string-list flags "load") ws-id)
    (let
        ((args (dict "action" action "path" parsed-path)))
      (when-let
          ((nn (or (flag-get flags "new-node") (flag-get flags "code"))))
        (setf (gethash "new_node" args) nn))
      (set-common-mutation-flags args flags)
      (let
          ((out (run-mcp-tool "ast_remove" args :ws-id ws-id)))
        (maybe-auto-commit flags ws-id)
        (values out 0 nil)))))

(defun cmd-relocate (positionals flags ws-id)
  "Mirror ast_relocate."
  (let*
      ((action (or (flag-get flags "action") (first positionals)))
       (src-str
         (or (flag-get flags "source-path") (flag-get flags "from") (second positionals)))
       (tgt-str
         (or (flag-get flags "target-path") (flag-get flags "to") (third positionals)))
       (src (parse-cli-path src-str))
       (tgt (parse-cli-path tgt-str)))
    (unless (and action src)
      (error
        "Usage: semcp relocate <move|copy|swap|merge|split> <source-path> [target-path]"))
    (ensure-loaded-paths (flag-string-list flags "load") ws-id)
    (let
        ((args (dict "action" action "source_path" src)))
      (when tgt (setf (gethash "target_path" args) tgt))
      (when-let ((ti (flag-int flags "target-index")))
        (setf (gethash "target_index" args) ti))
      (when-let ((idx (flag-int flags "index"))) (setf (gethash "index" args) idx))
      (when-let ((si (flag-int flags "split-index")))
        (setf (gethash "split_index" args) si))
      (when-let ((sep (flag-get flags "separator")))
        (setf (gethash "separator" args) sep))
      (set-common-mutation-flags args flags)
      (let
          ((out (run-mcp-tool "ast_relocate" args :ws-id ws-id)))
        (maybe-auto-commit flags ws-id)
        (values out 0 nil)))))

(defun cmd-search (positionals flags ws-id)
  "Mirror ast_search."
  (let*
      ((query (or (flag-get flags "query") (first positionals)))
       (extra-paths
         (if (flag-get flags "query")
           positionals
           (rest positionals))))
    (unless query (error "Usage: semcp search <query> [files-or-dirs...]"))
    (ensure-loaded-paths
      (or (flag-string-list flags "load") extra-paths)
      ws-id
      :default-cwd-p
      t)
    (let ((args (dict "query" query)))
      (when-let
          ((p (parse-cli-path (flag-get flags "path"))))
        (setf (gethash "path" args) p))
      (values (run-mcp-tool "ast_search" args :ws-id ws-id) 0 nil))))

(defun cmd-rename (positionals flags ws-id)
  "Mirror ast_rename."
  (let*
      ((old-name (or (flag-get flags "old-name") (first positionals)))
       (new-name (or (flag-get flags "new-name") (second positionals)))
       (extra-paths
         (if
             (and (flag-get flags "old-name") (flag-get flags "new-name"))
           positionals
           (cddr positionals))))
    (unless (and old-name new-name)
      (error "Usage: semcp rename <old-name> <new-name> [files-or-dirs...]"))
    (ensure-loaded-paths (or (flag-string-list flags "load") extra-paths) ws-id)
    (let
        ((args (dict "old_name" old-name "new_name" new-name)))
      (set-common-analysis-flags args flags)
      (set-common-mutation-flags args flags)
      (let
          ((out (run-mcp-tool "ast_rename" args :ws-id ws-id)))
        (maybe-auto-commit flags ws-id)
        (values out 0 nil)))))

(defun cmd-replace-pattern
       (positionals flags ws-id)
  "Mirror ast_replace_pattern."
  (let*
      ((pattern (or (flag-get flags "pattern") (first positionals)))
       (replacement (or (flag-get flags "replacement") (second positionals)))
       (extra-paths
         (if
             (and (flag-get flags "pattern") (flag-get flags "replacement"))
           positionals
           (cddr positionals))))
    (unless (and pattern replacement)
      (error
        "Usage: semcp replace-pattern <pattern> <replacement> [files-or-dirs...]"))
    (ensure-loaded-paths (or (flag-string-list flags "load") extra-paths) ws-id)
    (let
        ((args (dict "pattern" pattern "replacement" replacement)))
      (set-common-mutation-flags args flags)
      (let
          ((out (run-mcp-tool "ast_replace_pattern" args :ws-id ws-id)))
        (maybe-auto-commit flags ws-id)
        (values out 0 nil)))))

(defun cmd-extract-var (positionals flags ws-id)
  "Mirror ast_extract_variable."
  (let*
      ((path-str (or (flag-get flags "path") (first positionals)))
       (var-name
         (or (flag-get flags "name")
             (flag-get flags "var")
             (flag-get flags "variable-name")
             (second positionals)))
       (parsed-path (parse-cli-path path-str)))
    (unless (and parsed-path var-name)
      (error "Usage: semcp extract-var <path> <variable-name>"))
    (ensure-loaded-paths (flag-string-list flags "load") ws-id)
    (let
        ((args (dict "path" parsed-path "variable_name" var-name)))
      (when-let ((d (flag-get flags "dialect"))) (setf (gethash "dialect" args) d))
      (set-common-mutation-flags args flags)
      (let
          ((out (run-mcp-tool "ast_extract_variable" args :ws-id ws-id)))
        (maybe-auto-commit flags ws-id)
        (values out 0 nil)))))

(defun cmd-extract-fn (positionals flags ws-id)
  "Mirror ast_extract_function."
  (let*
      ((path-str (or (flag-get flags "path") (first positionals)))
       (fn-name
         (or (flag-get flags "name")
             (flag-get flags "fn")
             (flag-get flags "function-name")
             (second positionals)))
       (parsed-path (parse-cli-path path-str)))
    (unless (and parsed-path fn-name)
      (error "Usage: semcp extract-fn <path> <function-name> [--params a,b]"))
    (ensure-loaded-paths (flag-string-list flags "load") ws-id)
    (let
        ((args (dict "path" parsed-path "function_name" fn-name)))
      (when-let
          ((params (flag-string-list flags "params")))
        (setf (gethash "params" args) params))
      (when-let ((d (flag-get flags "dialect"))) (setf (gethash "dialect" args) d))
      (set-common-mutation-flags args flags)
      (let
          ((out (run-mcp-tool "ast_extract_function" args :ws-id ws-id)))
        (maybe-auto-commit flags ws-id)
        (values out 0 nil)))))

(defun lint-output-triggers-fail-p
       (output fail-on)
  "Return T if lint OUTPUT contains findings matching FAIL-ON threshold."
  (when (and fail-on (stringp output))
    (let
        ((level (string-downcase fail-on))
         (no-issues
           (or
             (search "No lint issues found" output :test #'char-equal)
             (search "0 issue" output :test #'char-equal))))
      (unless no-issues
        (cond
          ((member level '("any" "info" "style") :test #'string=) t)
          ((string= level "warning")
            (or
              (search "[WARNING]" output :test #'char-equal)
              (search "[ERROR]" output :test #'char-equal)))
          ((string= level "error") (search "[ERROR]" output :test #'char-equal))
          (t nil))))))

(defun cmd-lint (positionals flags ws-id)
  "Mirror and extend ast_lint (supports --fail-on for CI/hooks)."
  (ensure-loaded-paths
    (or (flag-string-list flags "load") positionals)
    ws-id
    :default-cwd-p
    t)
  (let ((args (dict)))
    (set-common-analysis-flags args flags)
    (when-let
        ((rules (flag-string-list flags "rules")))
      (setf (gethash "rules" args) rules))
    (let*
        ((out (run-mcp-tool "ast_lint" args :ws-id ws-id))
         (fail-on (flag-get flags "fail-on"))
         (exit-code
           (if (lint-output-triggers-fail-p out fail-on)
             2
             0)))
      (values out exit-code nil))))

(defun cmd-complexity (positionals flags ws-id)
  "Mirror ast_complexity_metrics."
  (ensure-loaded-paths
    (or (flag-string-list flags "load") positionals)
    ws-id
    :default-cwd-p
    t)
  (let
      ((args
         (dict "min_complexity"
               (flag-int flags "min-complexity" 1)
               "min_depth"
               (flag-int flags "min-depth" 1))))
    (set-common-analysis-flags args flags)
    (values (run-mcp-tool "ast_complexity_metrics" args :ws-id ws-id) 0 nil)))

(defun cmd-duplicates (positionals flags ws-id)
  "Mirror ast_find_duplicates."
  (ensure-loaded-paths
    (or (flag-string-list flags "load") positionals)
    ws-id
    :default-cwd-p
    t)
  (let
      ((args
         (dict "min_size"
               (flag-int flags "min-size" 3)
               "min_occurrences"
               (flag-int flags "min-occurrences" 2))))
    (when-let
        ((p (parse-cli-path (flag-get flags "path"))))
      (setf (gethash "path" args) p))
    (values (run-mcp-tool "ast_find_duplicates" args :ws-id ws-id) 0 nil)))

(defun cmd-bindings (positionals flags ws-id)
  "Mirror ast_analyze_bindings."
  (ensure-loaded-paths
    (or (flag-string-list flags "load") positionals)
    ws-id
    :default-cwd-p
    t)
  (let ((args (dict)))
    (set-common-analysis-flags args flags)
    (values (run-mcp-tool "ast_analyze_bindings" args :ws-id ws-id) 0 nil)))

(defun cmd-suggest (positionals flags ws-id)
  "Mirror ast_suggest_refactorings."
  (ensure-loaded-paths
    (or (flag-string-list flags "load") positionals)
    ws-id
    :default-cwd-p
    t)
  (let
      ((args (dict "min_priority" (flag-get flags "min-priority" "low"))))
    (set-common-analysis-flags args flags)
    (values (run-mcp-tool "ast_suggest_refactorings" args :ws-id ws-id) 0 nil)))

(defun cmd-create-file (positionals flags ws-id)
  "Mirror workspace_create_file."
  (let*
      ((fpath
         (or (flag-get flags "file")
             (flag-get flags "filepath")
             (flag-get flags "path")
             (first positionals)))
       (content (or (resolve-code-input (second positionals) flags) "")))
    (unless fpath
      (error "Usage: semcp create-file <filepath> [--content <str> | --stdin]"))
    (let
        ((args (dict "filepath" fpath "content" content)))
      (when-let ((d (flag-get flags "dialect"))) (setf (gethash "dialect" args) d))
      (let
          ((out (run-mcp-tool "workspace_create_file" args :ws-id ws-id)))
        (maybe-auto-commit flags ws-id)
        (values out 0 nil)))))

(defun cmd-status (flags ws-id)
  "Mirror workspace_status."
  (ensure-loaded-paths (flag-string-list flags "load") ws-id)
  (values (run-mcp-tool "workspace_status" (dict) :ws-id ws-id) 0 nil))

(defun cmd-diff (positionals flags ws-id)
  "Mirror workspace_diff."
  (let
      ((ws-a (or (flag-get flags "source") (first positionals) ws-id))
       (ws-b (or (flag-get flags "target") (second positionals) "default")))
    (values
      (run-mcp-tool "workspace_diff"
                    (dict "source_workspace_id" ws-a "target_workspace_id" ws-b)
                    :ws-id
                    ws-id)
      0
      nil)))

(defun cmd-rebase (positionals flags ws-id)
  "Mirror workspace_rebase."
  (let
      ((src (or (flag-get flags "source") (first positionals) ws-id))
       (onto (or (flag-get flags "onto") (flag-get flags "target") "default"))
       (strategy (flag-get flags "strategy" "three-way")))
    (values
      (run-mcp-tool "workspace_rebase"
                    (dict "source_workspace_id" src "target_workspace_id" onto "strategy" strategy)
                    :ws-id
                    ws-id)
      0
      nil)))

(defun cmd-merge (positionals flags ws-id)
  "Mirror workspace_merge."
  (let
      ((src (or (flag-get flags "source") (first positionals)))
       (tgt (or (flag-get flags "into") (flag-get flags "target") ws-id))
       (files (flag-string-list flags "files")))
    (unless src
      (error "Usage: semcp merge <source-workspace> [--into <target-workspace>]"))
    (let
        ((args (dict "source_workspace_id" src "target_workspace_id" tgt)))
      (when files (setf (gethash "files" args) files))
      (values (run-mcp-tool "workspace_merge" args :ws-id ws-id) 0 nil))))

(defun cmd-commit (positionals flags ws-id)
  "Mirror commit_workspace."
  (let
      ((files (or (flag-string-list flags "files") positionals)) (args (dict)))
    (when files (setf (gethash "files" args) files))
    (values (run-mcp-tool "commit_workspace" args :ws-id ws-id) 0 nil)))

(defun cmd-ws (positionals flags ws-id)
  "Mirror workspace_manage."
  (let*
      ((action (or (flag-get flags "action") (first positionals) "list"))
       (arg1 (second positionals))
       (args (dict "action" action)))
    (string-case (string-downcase action)
                 ("fork"
                  (let
                      ((tgt (or (flag-get flags "target") arg1))
                       (src (or (flag-get flags "from") (flag-get flags "source") ws-id)))
                    (unless tgt (error "Usage: semcp ws fork <target-id> [--from <source-id>]"))
                    (setf (gethash "target_id" args) tgt (gethash "source_id" args) src)))
                 (("create" "delete" "clear")
                  (when arg1 (setf (gethash "target_id" args) arg1))
                  (when (string= (string-downcase action) "delete")
                    (setf (gethash "workspace_id" args) (or arg1 ws-id)))
                  (when (string= (string-downcase action) "clear")
                    (setf (gethash "workspace_id" args) (or arg1 ws-id))))
                 (("snapshot" "restore")
                  (setf (gethash "snapshot_name" args)
                        (or arg1 (flag-get flags "name") "checkpoint")))
                 ("reload"
                  (let
                      ((files (or (flag-string-list flags "files") (rest positionals))))
                    (when files (setf (gethash "files" args) files)))))
    (when (flag-bool flags "force") (setf (gethash "force" args) t))
    (values (run-mcp-tool "workspace_manage" args :ws-id ws-id) 0 nil)))

(defun cmd-call (positionals flags ws-id)
  "Directly invoke any MCP tool by name with JSON arguments."
  (let*
      ((tool-name (first positionals))
       (json-str
         (cond
           ((flag-bool flags "stdin") (read-stdin-string))
           ((second positionals))
           (t "{}")))
       (yason:*parse-json-arrays-as-vectors* nil))
    (unless tool-name (error "Usage: semcp call <tool-name> [json-args | --stdin]"))
    (let ((args (yason:parse json-str)))
      (unless (hash-table-p args) (error "Tool arguments must be a JSON object."))
      (let
          ((out (run-mcp-tool tool-name args :ws-id ws-id)))
        (maybe-auto-commit flags ws-id)
        (values out 0 nil)))))

(defun cmd-tools (flags)
  "List all available MCP tools and their descriptions."
  (let ((tools (get-tools-list)))
    (if (flag-bool flags "json")
      (values (json-encode-to-string (dict "ok" t "tools" tools)) 0 nil)
      (let
          ((out
             (with-output-to-string (s)
               (format s "Available MCP Tools (~D):~%~%" (length tools))
               (dolist (tool tools)
                 (format s
                         "  ~26A ~A~%"
                         (href tool "name")
                         (first (split-string-by-char (or (href tool "description") "") #\newline)))))))
        (values out 0 nil)))))

(defun resolve-daemon-session-dir
       (flags &optional create-p)
  "Resolve the .semcp directory for daemon commands."
  (let*
      ((explicit (or (flag-get flags "session") (flag-get flags "state-dir")))
       (dir
         (if explicit
           (uiop:ensure-directory-pathname explicit)
           (or (find-session-dir)
               (merge-pathnames (concatenate 'string *session-dir-name* "/") (uiop:getcwd))))))
    (when create-p (ensure-directories-exist (daemon-socket-path dir)))
    dir))

(defun cmd-daemon (positionals flags)
  "Manage the in-RAM workspace daemon (start, stop, status, save)."
  (let
      ((action (string-downcase (or (first positionals) "status"))))
    (string-case action
                 ("start"
                  (let
                      ((dir (resolve-daemon-session-dir flags t))
                       (load-paths (or (rest positionals) (flag-string-list flags "load"))))
                    (when (daemon-ping dir)
                      (return-from cmd-daemon
                                   (values
                                     (fmt "Daemon is already running on ~A." (namestring (daemon-socket-path dir)))
                                     0
                                     nil)))
                    (when
                        (and load-paths (not (probe-file (session-state-file dir))))
                      (init-workspace)
                      (load-into-workspace load-paths)
                      (save-session dir))
                    (if (flag-bool flags "foreground")
                      (progn (run-daemon-foreground dir) (values "Daemon stopped." 0 nil))
                      (let ((info (start-daemon-background dir)))
                        (values
                          (fmt
                            "Started semcp daemon (PID ~A) on ~A (~A workspace~:P, ~A file~:P)."
                            (href info "pid")
                            (namestring (daemon-socket-path dir))
                            (href info "workspaces")
                            (href info "files"))
                          0
                          info)))))
                 ("status"
                  (let*
                      ((dir (resolve-daemon-session-dir flags nil)) (info (daemon-ping dir)))
                    (if info
                      (values
                        (fmt
                          "Daemon running (PID ~A) on ~A (~A workspace~:P, ~A file~:P)."
                          (href info "pid")
                          (namestring (daemon-socket-path dir))
                          (href info "workspaces")
                          (href info "files"))
                        0
                        info)
                      (values "Daemon is not running." 1 (dict "running" yason:false)))))
                 ("save"
                  (let*
                      ((dir (resolve-daemon-session-dir flags nil))
                       (sock (daemon-socket-path dir))
                       (resp
                         (when (probe-file sock) (daemon-send-request sock (dict "action" "save")))))
                    (unless resp (error "Daemon is not running."))
                    (values (fmt "Saved daemon state to ~A." (href resp "state_file")) 0 resp)))
                 ("stop"
                  (let*
                      ((dir (resolve-daemon-session-dir flags nil))
                       (sock (daemon-socket-path dir))
                       (resp
                         (when (probe-file sock) (daemon-send-request sock (dict "action" "stop")))))
                    (if resp
                      (values "Stopped semcp daemon (state saved)." 0 resp)
                      (values "Daemon is not running." 0 (dict "stopped" yason:false)))))
                 (t
                   (error "Unknown daemon action ~S. Expected: start, stop, status, save." action)))))

(defun cmd-completions (positionals)
  "Generate shell completion script for bash, zsh, or fish."
  (let
      ((shell (string-downcase (or (first positionals) "bash")))
       (cmds
         "init load read slice modify remove delete unwrap promote relocate search rename replace-pattern extract-var extract-fn lint complexity duplicates bindings suggest create-file status diff rebase merge commit ws workspace call tools completions mcp serve help version"))
    (string-case shell
                 ("bash"
                  (values
                    (format nil
                            "_semcp_completions() {~%  local cur=\"${COMP_WORDS[COMP_CWORD]}\"~%  if [[ ${COMP_CWORD} -eq 1 ]]; then~%    COMPREPLY=( $(compgen -W \"~A\" -- \"${cur}\") )~%  fi~%}~%complete -F _semcp_completions semcp~%"
                            cmds)
                    0
                    nil))
                 ("zsh"
                  (values
                    (format nil
                            "#compdef semcp~%_semcp() {~%  local -a commands~%  commands=(~{~%    '~A'~%~}  )~%  _describe 'command' commands~%}~%_semcp \"$@\"~%"
                            (split-string-by-char cmds #\space))
                    0
                    nil))
                 ("fish"
                  (values
                    (with-output-to-string (s)
                      (dolist (c (split-string-by-char cmds #\space))
                        (format s "complete -c semcp -n '__fish_use_subcommand' -a '~A'~%" c)))
                    0
                    nil))
                 (t (error "Unsupported shell ~S. Supported: bash, zsh, fish." shell)))))