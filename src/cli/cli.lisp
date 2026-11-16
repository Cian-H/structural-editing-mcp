(in-package :structural-editing-mcp.cli)

(declaim (optimize (speed 2) (safety 3)))

(defparameter *cli-subcommand-help*
  '(("daemon" .
     "Usage: semcp daemon <start|stop|status|save> [paths...] [--foreground] [--session <dir>]

Manage a background daemon that holds *workspace-registry* in RAM over a
Unix domain socket (.semcp/daemon.sock) for sub-millisecond CLI execution.

Subcommands:
  start [paths...]  Start background daemon (or --foreground)
  status            Check if daemon is running and show workspace/file counts
  save              Flush in-RAM state to .semcp/state.json
  stop              Save state to .semcp/state.json and stop the daemon")
    ("init" .
     "Usage: semcp init [paths...] [--empty] [--session <dir>]

Initialize a persistent .semcp/ staged workspace session in the current
directory (or --session). Loads [paths...] (default: current directory) into
the 'default' workspace and saves .semcp/state.json.

Once initialized, all subsequent semcp commands automatically load and save
this session state across invocations until you run 'semcp commit'.

Examples:
  semcp init src/
  semcp init --empty")
    ("load" .
     "Usage: semcp load <file-or-dir>... [-w <ws>]

Load additional Lisp files or directories into the active workspace session.

Examples:
  semcp load test/
  semcp load src/extra.lisp -w feat-1")
    ("read" .
     "Usage: semcp read [file|path] [-d <depth>] [-m <full|skeleton|auto>] [-n <limit>] [-o <offset>] [-L <path>]

Inspect an AST node, file, or workspace (mirrors MCP 'read_node').
If [file|path] is an existing file on disk, it is loaded and displayed directly.
Otherwise it is parsed as an AST path (e.g. 0,0,2, #n42, main.lisp/defun:main).

Examples:
  semcp read src/main.lisp
  semcp read 0,0,2 --depth 3
  semcp read --mode skeleton")
    ("slice" .
     "Usage: semcp slice <path> [-d <depth>] [-m <full|skeleton|auto>] [-L <path>]

Cast a vertical ray down to <path>, eliding lateral siblings at ancestor
levels while hydrating the target node (mirrors MCP 'read_slice').

Examples:
  semcp slice 0,0,5,2
  semcp slice main.lisp/defun:main/3 -L src/")
    ("modify" .
     "Usage: semcp modify <insert|overwrite|wrap> <path> [<code>] [-i|--commit]

Mutate an AST node in memory (mirrors MCP 'ast_modify').
  insert     Insert <code> before <path> (or at --index <n>)
  overwrite  Replace node at <path> with <code>
  wrap       Wrap node (or --index..--end-index range) with parens or form

Options:
  --index <n>              Child index for insert or range wrap start
  --end-index <n>          Ending child index for range wrap
  --expected-revision <n>  OCC guard: abort if workspace revision changed
  -F, --force              Bypass >50-child overwrite safeguard
  --stdin                  Read <code> from standard input
  -i, --commit             Persist immediately to disk (useful in one-shot mode)

Examples:
  semcp modify overwrite 0,0,2,1 new-name
  semcp modify wrap 0,0,3 '(when ok)' -L app.lisp -i")
    ("remove" .
     "Usage: semcp remove <delete|unwrap|promote> <path> [-i|--commit]
       semcp delete <path> | semcp unwrap <path> | semcp promote <path>

Remove, unwrap, or promote AST nodes (mirrors MCP 'ast_remove').
  delete   Remove node at <path>
  unwrap   Strip enclosing parens/brackets, spilling children into parent
  promote  Replace parent node with child at <path>

Examples:
  semcp delete 0,0,4
  semcp unwrap 0,0,2,3 -L app.lisp -i")
    ("relocate" .
     "Usage: semcp relocate <move|copy|swap|merge|split> <source-path> [target-path] [options]

Reorder, duplicate, swap, merge, or split AST nodes (mirrors MCP 'ast_relocate').
Options: --index <n>, --target-index <n>, --split-index <n>, --separator <str>.")
    ("search" .
     "Usage: semcp search <query> [paths...] [-p <scope-path>]

Search the workspace or a subtree for symbols, calls, or literals (mirrors MCP 'ast_search').

Examples:
  semcp search defun src/
  semcp search handle-message -p 0,0")
    ("rename" .
     "Usage: semcp rename <old-name> <new-name> [paths...] [-p <scope>] [-i|--commit]

Semantically rename symbol leaf nodes, preserving strings and comments (mirrors MCP 'ast_rename').")
    ("replace-pattern" .
     "Usage: semcp replace-pattern <pattern> <replacement> [paths...] [-i|--commit]

Wildcard AST pattern rewrite using ?var placeholders (mirrors MCP 'ast_replace_pattern').

Example:
  semcp replace-pattern '(old-fn ?x ?y)' '(new-fn ?y :arg ?x)' src/ -i")
    ("extract-var" .
     "Usage: semcp extract-var <path> <variable-name> [-i|--commit]

Extract AST node at <path> into a local let binding (mirrors MCP 'ast_extract_variable').")
    ("extract-fn" .
     "Usage: semcp extract-fn <path> <function-name> [--params p1,p2] [-i|--commit]

Extract AST node at <path> into a top-level function (mirrors MCP 'ast_extract_function').")
    ("lint" .
     "Usage: semcp lint [paths...] [--rules r1,r2] [--fail-on <error|warning|any>]

Run structural anti-pattern lint rules (mirrors MCP 'ast_lint').
Pass --fail-on to exit with status 2 when matching issues are found (for CI/hooks).")
    ("complexity" .
     "Usage: semcp complexity [paths...] [--min-complexity <n>] [--min-depth <n>]

Compute cyclomatic complexity and nesting depth metrics (mirrors MCP 'ast_complexity_metrics').")
    ("duplicates" .
     "Usage: semcp duplicates [paths...] [--min-size <n>] [--min-occurrences <n>]

Detect duplicate AST subtrees across files (mirrors MCP 'ast_find_duplicates').")
    ("bindings" .
     "Usage: semcp bindings [paths...] [-p <scope>]

Analyze lexical variable bindings, unused variables, and shadowing (mirrors MCP 'ast_analyze_bindings').")
    ("suggest" .
     "Usage: semcp suggest [paths...] [--min-priority <high|medium|low>]

Aggregate lint, complexity, duplicate, and binding findings into a refactoring plan.")
    ("ws" .
     "Usage: semcp ws <action> [args...]

Manage workspaces & branches (mirrors MCP 'workspace_manage'):
  list                    List all workspaces and revisions
  create <id>             Create a new empty workspace
  delete <id>             Delete a workspace
  clear [id]              Reset a workspace (pass -F/--force if dirty)
  fork <target> [--from]  Fork an isolated branch workspace
  snapshot [name]         Create a named checkpoint
  restore [name]          Rollback to a named checkpoint
  reload [files...]       Reload files from disk")
    ("create-file" .
     "Usage: semcp create-file <filepath> [--content <str> | --stdin] [-i|--commit]

Stage a new source file in the in-memory workspace without touching disk.")
    ("status" .
     "Usage: semcp status [-w <ws>]

Show dirty/clean files, current/base revisions, and snapshots.")
    ("diff" .
     "Usage: semcp diff [ws-a] [ws-b]

Compute AST and file-level differences between two workspaces.")
    ("rebase" .
     "Usage: semcp rebase [source-ws] [--onto <target-ws>] [--strategy <three-way|theirs|ours|error>]

Rebase a branch workspace onto another workspace with 3-way AST merge.")
    ("merge" .
     "Usage: semcp merge <source-ws> [--into <target-ws>] [--files f1,f2]

Merge changes from <source-ws> into <target-ws> using AST-level disjoint merge.")
    ("commit" .
     "Usage: semcp commit [files...] [-w <ws>]

Write dirty/modified workspace files to disk (clean files are untouched).")
    ("call" .
     "Usage: semcp call <mcp-tool-name> [json-args | --stdin]

Invoke any MCP tool directly by name with a JSON arguments object.

Example:
  semcp call read_node '{\"path\": [0, 0], \"mode\": \"skeleton\"}' --json")
    ("tools" .
     "Usage: semcp tools [--json]

List all 22 MCP tools (pass --json for full JSON schemas).")
    ("completions" .
     "Usage: semcp completions <bash|zsh|fish>

Output a shell completion script for semcp.")
    ("mcp" .
     "Usage: semcp [mcp | serve]

Start the Model Context Protocol (MCP) JSON-RPC 2.0 server over stdio."))
  "Detailed help text per CLI subcommand.")

(defun format-cli-help (&optional topic)
  "Return help text for the semcp CLI (or a specific TOPIC)."
  (when topic
    (let*
        ((key (string-downcase topic))
         (normalized
           (cond
             ((string= key "workspace") "ws")
             ((string= key "serve") "mcp")
             ((string= key "exec") "call")
             ((member key '("delete" "unwrap" "promote") :test #'string=) "remove")
             (t key)))
         (entry (assoc normalized *cli-subcommand-help* :test #'string=)))
      (when entry (return-from format-cli-help (format nil "~A~%" (cdr entry))))))
  (with-output-to-string (s)
    (format s
            "semcp ~A — Structural Editing & Static Analysis CLI / MCP Server~%~%"
            +version+)
    (format s "Usage:~%")
    (format s
            "  semcp                          Start MCP stdio server (default when no args)~%")
    (format s
            "  semcp <command> [args] [flags] Run a structural editing or analysis command~%")
    (format s
            "  semcp <command> --help         Show detailed help & examples for a command~%~%")
    (format s "How Persistence Works (Staged Sessions vs One-Shot):~%")
    (format s "  1. Staged Session (.semcp/state.json):~%")
    (format s
            "     Run 'semcp init [paths...]' to create .semcp/ in your project. Subsequent~%")
    (format s
            "     commands auto-discover .semcp/, stage AST edits and branches in memory,~%")
    (format s
            "     and persist state between CLI calls until you run 'semcp commit'.~%")
    (format s "  2. One-Shot Mode (no .semcp/ or --no-session):~%")
    (format s
            "     Pass files/dirs directly (e.g. 'semcp lint src/'). Mutations preview~%")
    (format s
            "     in-memory unless -i / --commit is passed to write changes to disk.~%~%")
    (format s "AST Path Addressing:~%")
    (format s
            "  0,0,2 | [0,0,2] | 0.0.2        0-indexed [dialect, file, form, ...] coordinates~%")
    (format s
            "  #n42                           Node handle ID shown in 'read' / 'slice' output~%")
    (format s "  main.lisp/defun:main/2         Symbolic path segments~%~%")
    (format s "Session & Inspection:~%")
    (format s
            "  init [paths...]                Initialize .semcp/ staged workspace session~%")
    (format s
            "  load <paths...>                Load files/directories into active session~%")
    (format s
            "  read [file|path]               Inspect AST node or file (alias: read_node)~%")
    (format s
            "  slice <path>                   Inspect vertical AST ray spine (alias: read_slice)~%")
    (format s
            "  search <query> [paths...]      AST-aware symbol/token search (alias: ast_search)~%~%")
    (format s "AST Surgery & Refactoring:~%")
    (format s
            "  modify <act> <path> <code>     Insert, overwrite, or wrap AST node (alias: ast_modify)~%")
    (format s
            "  remove <act> <path>            Delete, unwrap, or promote (aliases: delete, unwrap, promote)~%")
    (format s
            "  relocate <act> <src> [tgt]     Move, copy, swap, merge, or split (alias: ast_relocate)~%")
    (format s
            "  rename <old> <new> [paths...]  Semantic symbol rename (alias: ast_rename)~%")
    (format s
            "  replace-pattern <pat> <rep>    Wildcard AST pattern rewrite (alias: ast_replace_pattern)~%")
    (format s
            "  extract-var <path> <name>      Extract node into let binding (alias: ast_extract_variable)~%")
    (format s
            "  extract-fn <path> <name>       Extract node into top-level defun (alias: ast_extract_function)~%~%")
    (format s "Static Analysis:~%")
    (format s
            "  lint [paths...]                Run structural lint rules (supports --fail-on <level>)~%")
    (format s
            "  complexity [paths...]          Cyclomatic complexity & nesting metrics~%")
    (format s "  duplicates [paths...]          Detect duplicate AST subtrees~%")
    (format s
            "  bindings [paths...]            Lexical variable & shadowing analysis~%")
    (format s "  suggest [paths...]             Prioritized refactoring plan~%~%")
    (format s "Workspace & Branching:~%")
    (format s
            "  ws <action> [args...]          Manage workspaces (list, create, fork, snapshot, restore, ...)~%")
    (format s "  create-file <file> [content]   Stage a new file in workspace~%")
    (format s
            "  status                         Show dirty/clean files and revisions~%")
    (format s "  diff [ws-a] [ws-b]             Diff workspaces or against base~%")
    (format s "  rebase [src] [--onto tgt]      Rebase branch workspace~%")
    (format s "  merge <src> [--into tgt]       AST-level disjoint merge~%")
    (format s
            "  commit [files...]              Write dirty workspace files to disk~%~%")
    (format s "Utilities & Server:~%")
    (format s
            "  call <tool> [json|--stdin]     Invoke any MCP tool directly with JSON args~%")
    (format s
            "  tools                          List all MCP tools (use --json for schemas)~%")
    (format s "  completions <bash|zsh|fish>    Generate shell completion script~%")
    (format s "  mcp | serve                    Start MCP stdio server~%")
    (format s "  version | -v | --version       Print version~%")
    (format s
            "  help | -h | --help [cmd]       Print this help or command-specific help~%~%")
    (format s "Global Flags:~%")
    (format s
            "  -w, --workspace <id>           Target workspace ID (default: default)~%")
    (format s
            "  -L, --load <path>              Load file or directory before running~%")
    (format s
            "  -i, --commit, --in-place       Persist mutations to disk immediately~%")
    (format s
            "      --dry-run                  Preview without saving session or disk~%")
    (format s
            "      --no-session               Ignore .semcp/ session (ephemeral mode)~%")
    (format s
            "      --session <dir>            Use explicit .semcp/ session directory~%")
    (format s "      --json                     Output structured JSON~%")
    (format s
            "      --stdin                    Read code/JSON payload from stdin~%")))

(defparameter *session-mutating-commands*
  '("load" "modify"
    "ast_modify"
    "remove"
    "ast_remove"
    "delete"
    "unwrap"
    "promote"
    "relocate"
    "ast_relocate"
    "rename"
    "ast_rename"
    "replace-pattern"
    "ast_replace_pattern"
    "extract-var"
    "ast_extract_variable"
    "extract-fn"
    "ast_extract_function"
    "create-file"
    "workspace_create_file"
    "rebase"
    "workspace_rebase"
    "merge"
    "workspace_merge"
    "commit"
    "commit_workspace"
    "ws"
    "workspace"
    "workspace_manage"
    "call"
    "exec")
  "Commands that may mutate workspace registry state and should persist session.")

(defun resolve-active-session-dir (flags)
  "Resolve the active .semcp session directory unless --no-session is set."
  (unless (flag-bool flags "no-session")
    (let
        ((explicit (or (flag-get flags "session") (flag-get flags "state-dir"))))
      (if explicit
        (let
            ((dir (uiop:ensure-directory-pathname explicit)))
          (when (probe-file (session-state-file dir)) dir))
        (find-session-dir)))))

(defun dispatch-cli-command
       (cmd positionals flags ws-id session-dir)
  "Dispatch normalized CLI command CMD. Returns (values text exit-code extra-json)."
  (string-case cmd
               ("init" (cmd-init positionals flags ws-id session-dir))
               ("load" (cmd-load positionals flags ws-id))
               (("read" "read_node") (cmd-read positionals flags ws-id))
               (("slice" "read_slice") (cmd-slice positionals flags ws-id))
               (("modify" "ast_modify") (cmd-modify positionals flags ws-id))
               (("remove" "ast_remove") (cmd-remove nil positionals flags ws-id))
               ("delete" (cmd-remove "delete" positionals flags ws-id))
               ("unwrap" (cmd-remove "unwrap" positionals flags ws-id))
               ("promote" (cmd-remove "promote" positionals flags ws-id))
               (("relocate" "ast_relocate") (cmd-relocate positionals flags ws-id))
               (("search" "ast_search") (cmd-search positionals flags ws-id))
               (("rename" "ast_rename") (cmd-rename positionals flags ws-id))
               (("replace-pattern" "ast_replace_pattern")
                (cmd-replace-pattern positionals flags ws-id))
               (("extract-var" "ast_extract_variable")
                (cmd-extract-var positionals flags ws-id))
               (("extract-fn" "ast_extract_function") (cmd-extract-fn positionals flags ws-id))
               (("lint" "ast_lint") (cmd-lint positionals flags ws-id))
               (("complexity" "ast_complexity_metrics")
                (cmd-complexity positionals flags ws-id))
               (("duplicates" "ast_find_duplicates") (cmd-duplicates positionals flags ws-id))
               (("bindings" "ast_analyze_bindings") (cmd-bindings positionals flags ws-id))
               (("suggest" "ast_suggest_refactorings") (cmd-suggest positionals flags ws-id))
               (("create-file" "workspace_create_file")
                (cmd-create-file positionals flags ws-id))
               (("status" "workspace_status") (cmd-status flags ws-id))
               (("diff" "workspace_diff") (cmd-diff positionals flags ws-id))
               (("rebase" "workspace_rebase") (cmd-rebase positionals flags ws-id))
               (("merge" "workspace_merge") (cmd-merge positionals flags ws-id))
               (("commit" "commit_workspace") (cmd-commit positionals flags ws-id))
               (("ws" "workspace" "workspace_manage") (cmd-ws positionals flags ws-id))
               (("call" "exec") (cmd-call positionals flags ws-id))
               ("tools" (cmd-tools flags))
               ("completions" (cmd-completions positionals))
               ("daemon" (cmd-daemon positionals flags))
               (t (error "Unknown command: ~A. Run 'semcp --help' for usage." cmd))))

(defun format-cli-output
       (cmd ws-id text exit-code extra-json flags)
  "Format command output as plain text or JSON depending on --json."
  (if
      (and (flag-bool flags "json") (not (string= cmd "tools")))
    (let
        ((obj
           (dict "ok"
                 (zerop exit-code)
                 "command"
                 cmd
                 "workspace"
                 ws-id
                 "exit_code"
                 exit-code
                 "result"
                 text)))
      (when (hash-table-p extra-json)
        (maphash
          (lambda (k v)
            (setf (gethash k obj) v))
          extra-json))
      (json-encode-to-string obj))
    text))

(defun execute-cli-in-ram (argv session-dir)
  "Execute CLI command ARGV directly against the live in-RAM *WORKSPACE-REGISTRY*.
Used by the background daemon to avoid disk reloads/saves per command."
  (handler-case
      (multiple-value-bind (raw-cmd positionals flags)
                           (parse-cli-args argv)
        (let
            ((cmd (when raw-cmd (string-downcase raw-cmd)))
             (ws-id (flag-get flags "workspace" "default")))
          (multiple-value-bind (text exit-code extra-json)
                               (dispatch-cli-command cmd positionals flags ws-id session-dir)
            (values (format-cli-output cmd ws-id text exit-code extra-json flags) exit-code))))
    (error (e)
      (let
          ((json-mode (or (member "--json" argv :test #'string=) nil))
           (msg (format nil "~A" e)))
        (if json-mode
          (values (json-encode-to-string (dict "ok" yason:false "error" msg)) 1)
          (values (fmt "Error: ~A" msg) 1))))))

(defun execute-cli (argv)
  "Execute CLI with ARGV without terminating the process.
Returns (values output-string exit-code)."
  (handler-case
      (multiple-value-bind (raw-cmd positionals flags)
                           (parse-cli-args argv)
        (let
            ((cmd (when raw-cmd (string-downcase raw-cmd))))
          (cond
            ((flag-bool flags "version")
              (values (fmt "structural-editing-mcp ~A" +version+) 0))
            ((or (flag-bool flags "help") (string= cmd "help"))
              (values
                (format-cli-help
                  (if (and cmd (not (string= cmd "help")))
                    cmd
                    (first positionals)))
                0))
            ((string= cmd "version") (values (fmt "structural-editing-mcp ~A" +version+) 0))
            ((or (null cmd) (string= cmd "mcp") (string= cmd "serve"))
              (start-server)
              (values "" 0))
            (t
              ;; Fast path: forward to live in-RAM daemon if running
              (when-let
                  ((daemon-dir
                     (unless
                         (member cmd '("daemon" "init" "completions") :test #'string=)
                       (find-daemon-session-dir flags))))
                (multiple-value-bind (out code)
                                     (daemon-forward-cli daemon-dir argv)
                  (when out (return-from execute-cli (values out code)))))
              (let*
                  ((ws-id (flag-get flags "workspace" "default"))
                   (session-dir (resolve-active-session-dir flags)))
                (if session-dir
                  (load-session session-dir)
                  (reset-workspace-registry))
                (multiple-value-bind (text exit-code extra-json)
                                     (dispatch-cli-command cmd positionals flags ws-id session-dir)
                  (when
                      (and session-dir
                           (not (flag-bool flags "dry-run"))
                           (not (string= cmd "init"))
                           (member cmd *session-mutating-commands* :test #'string=))
                    (save-session session-dir))
                  (values (format-cli-output cmd ws-id text exit-code extra-json flags) exit-code)))))))
    (error (e)
      (let
          ((json-mode (or (member "--json" argv :test #'string=) nil))
           (msg (format nil "~A" e)))
        (if json-mode
          (values (json-encode-to-string (dict "ok" yason:false "error" msg)) 1)
          (values (fmt "Error: ~A" msg) 1))))))

(defun run-cli (argv)
  "Run the CLI and exit the process with the resulting exit code."
  (multiple-value-bind (output exit-code)
                       (execute-cli argv)
    (when (and output (plusp (length output)))
      (let
          ((stream
             (if (zerop exit-code)
               *standard-output*
               *error-output*)))
        (write-string output stream)
        (unless (char= (char output (1- (length output))) #\newline) (terpri stream))
        (force-output stream)))
    (uiop:quit exit-code)))