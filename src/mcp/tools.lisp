(in-package :structural-editing-mcp.mcp)

(declaim (optimize (speed 2) (safety 3)))

;;; ----------------------------------------------------------------------
;;; Tool Definitions (Unified Declarations & Handlers)
;;; ----------------------------------------------------------------------

(define-mcp-tool "read_node"
    (:description "Inspect any node in the AST or workspace. Returns rendered code and a nested tree of child paths up to 'depth' levels. Supports 'mode': 'skeleton' for compact metadata stubs on wide trees. BEST PRACTICE: Always read parent forms (e.g. [0, 10]) to see the entire expression and its child paths at once—do NOT probe child indices one-by-one. Use 'read_slice' to cast a vertical ray down to a specific nested node without lateral context blowout. Pass 'load_files' on initial call to populate the workspace from disk."
     :mutation nil)
    ((path :type :path)
     (depth :type :integer :doc "Recursion depth for displaying nested children and their paths (default: 2)." :default 2)
     (limit :type :integer :doc "Optional maximum number of child nodes or files to display in preview (default: 50)." :default 50)
     (offset :type :integer :doc "Optional 0-indexed child offset to start displaying from (default: 0)." :default 0)
     (mode :type :string :doc "Display mode: 'full' (default), 'skeleton' (compact metadata stubs for wide trees), or 'auto' (automatic skeleton for massive nodes)."
           :enum ("full" "skeleton" "auto") :default "auto")
     (load_files :type :load-files))
  (bt:with-lock-held (structural-editing-mcp.workspace:*workspace-lock*)
    (let ((files-to-load (to-list load_files)))
      (when files-to-load
        (structural-editing-mcp.workspace:load-into-workspace files-to-load)))
    (unless structural-editing-mcp.workspace:*workspace-tree*
      (structural-editing-mcp.workspace:init-workspace))
    (structural-editing-mcp.workspace:record-agent-read agent-id)
    (let ((node (structural-editing-mcp.tree:resolve-tree-scope
                 structural-editing-mcp.workspace:*workspace-tree* path)))
      (format-node-preview node :depth depth :limit limit :offset offset :mode mode))))

(define-mcp-tool "read_slice"
    (:description "Inspect a vertical ray/spine path down to a specific target AST node. Renders the ancestral hierarchy (e.g. file, let, defun) leading to the target node while strictly eliding lateral siblings at each level, then hydrates the target node up to 'depth'. Ideal for inspecting deeply nested code in wide trees without context window blowout."
     :mutation nil)
    ((path :type :path :doc "Target AST path (e.g. [0, 0, 50, 2]) to cast the vertical ray down to." :required t)
     (depth :type :integer :doc "Recursion depth for displaying nested children of the target node (default: 2)." :default 2)
     (limit :type :integer :doc "Optional maximum number of child nodes to display for the target node (default: 50)." :default 50)
     (offset :type :integer :doc "Optional 0-indexed child offset for the target node." :default 0)
     (mode :type :string :doc "Display mode for the target node: 'full' (default) or 'skeleton'."
           :enum ("full" "skeleton" "auto") :default "full")
     (load_files :type :load-files))
  (bt:with-lock-held (structural-editing-mcp.workspace:*workspace-lock*)
    (let ((files-to-load (to-list load_files)))
      (when files-to-load
        (structural-editing-mcp.workspace:load-into-workspace files-to-load)))
    (unless structural-editing-mcp.workspace:*workspace-tree*
      (structural-editing-mcp.workspace:init-workspace))
    (structural-editing-mcp.workspace:record-agent-read agent-id)
    (format-node-slice structural-editing-mcp.workspace:*workspace-tree* path args)))

(define-mcp-tool "ast_modify"
    (:description "Mutate AST nodes in memory. Actions: 'insert' (adds new_node before path, or at child index if index is given), 'overwrite' (replaces node at path with new_node), 'wrap' (wraps node or child range with parens, brackets, or enclosing form). WARNING: Always target the exact, specific child path (e.g. [0, 1, 5, 2]) for your mutation. DO NOT attempt to overwrite a parent node using a truncated child list, as this will delete all un-rendered siblings. NOTE: Automatically returns an updated preview of the enclosing parent node; separate verification reads are unnecessary. To preserve safety in multi-agent workflows, fork a workspace first with 'workspace_manage'."
     :mutation t)
    ((path :type :path :doc "Target AST path (e.g. [0, 2] to target form 2 in file 0)." :required t)
     (action :type :string :doc "The modification action to perform." :enum ("insert" "overwrite" "wrap") :required t)
     (new_node :type :string :doc "For insert/overwrite: the S-expression code string (e.g. '(defun foo () 42)'). For wrap: delimiter keyword (':paren', ':square', ':curly') or enclosing form string (e.g. '(when condition)')." :required t)
     (index :type :integer :doc "Optional child index for insert. If omitted when inserting, uses the last element of path.")
     (end_index :type :integer :doc "Optional ending child index for range wrapping with action 'wrap'.")
     (force :type :boolean :doc "Optional safeguard bypass: set to true to force overwriting a node that has more than 50 children."))
  (let ((tree structural-editing-mcp.workspace:*workspace-tree*))
    (when (equal action "overwrite")
      (let* ((target-node (structural-editing-mcp.tree:get-node-at-path tree path))
             (child-count (length (structural-editing-mcp.tree:get-node-children target-node))))
        (when (and (> child-count 50) (not force))
          (error "Safeguard: Node at path ~A has ~D children. Overwriting a large parent node directly will delete all its children. To modify an element inside, target the specific child path instead (e.g. append child index to path). If you genuinely intend to overwrite the entire collection, pass 'force': true."
                 path child-count))))
    (setf structural-editing-mcp.workspace:*workspace-tree*
          (string-case (or action "")
            ("insert"
             (perform-insert tree path new_node index))
            ("overwrite"
             (structural-editing-mcp.edit:overwrite-expression tree path new_node))
            ("wrap"
             (perform-wrap tree path new_node end_index index))
            (t (error "Unknown action: ~A" action))))
    (format-mutation-result (fmt "Successfully executed ~A at ~A" action path)
                            path
                            structural-editing-mcp.workspace:*workspace-tree*)))

(define-mcp-tool "ast_remove"
    (:description "Remove, unwrap, or promote AST nodes in memory. Actions: 'delete' (deletes the node at path), 'unwrap' (removes enclosing collection, spilling children into parent), 'promote' (replaces parent node with the child node at path). WARNING: Always target the exact, specific child path for removal. NOTE: Automatically returns an updated preview of the parent form."
     :mutation t)
    ((path :type :path :doc "The AST path of the node to remove/unwrap/promote." :required t)
     (action :type :string :doc "The removal action to perform." :enum ("delete" "unwrap" "promote") :required t)
     (new_node :type :string :doc "Optional replacement node string (for action 'replace' or fallback)."))
  (let ((tree structural-editing-mcp.workspace:*workspace-tree*))
    (setf structural-editing-mcp.workspace:*workspace-tree*
          (string-case (or action "")
            ("delete" (structural-editing-mcp.edit:delete-node tree path))
            ("unwrap" (structural-editing-mcp.edit:unwrap-node tree path))
            ("promote" (structural-editing-mcp.edit:promote-node tree path))
            (t (error "Unknown action: ~A" action))))
    (format-mutation-result (fmt "Successfully executed ~A at ~A" action path)
                            path
                            structural-editing-mcp.workspace:*workspace-tree*)))

(define-mcp-tool "ast_relocate"
    (:description "Reorder, copy, swap, merge, or split AST nodes and collections in memory. Actions: 'move' (relocates node from source_path to target_path or target_index), 'copy' (duplicates node from source_path into target_path), 'swap' (interchanges positions of two nodes at source_path and target_path), 'merge' (combines two collections or files into one), 'split' (divides a collection into two at split_index). NOTE: Automatically returns a preview of the enclosing form."
     :mutation t)
    ((action :type :string :doc "The relocation action to perform." :enum ("move" "copy" "swap" "merge" "split") :required t)
     (source_path :type :path :doc "Source AST path of the node to move, copy, swap, or merge.")
     (target_path :type :path :doc "Target AST path for move, copy, swap, or merge.")
     (target_index :type :integer :doc "Optional child index within target_path to insert the relocated node.")
     (index :type :integer :doc "Optional index within target.")
     (split_index :type :integer :doc "For action 'split': the 0-indexed child position at which to split the collection.")
     (separator :type :string :doc "Optional separator string for merge."))
  (let* ((src (or (to-list source_path) path))
         (tgt (to-list target_path))
         (idx (or target_index index)))
    (setf structural-editing-mcp.workspace:*workspace-tree*
          (perform-relocate-action action src tgt idx structural-editing-mcp.workspace:*workspace-tree*))
    (format-mutation-result (fmt "Successfully executed ~A from ~A to ~A" action src tgt)
                            tgt
                            structural-editing-mcp.workspace:*workspace-tree*)))

(define-mcp-tool "ast_search"
    (:description "Search the workspace or a subtree for symbols, identifiers, function calls, or literal values. Fast AST-aware token searching that returns matched AST paths."
     :mutation nil)
    ((query :type :string :doc "Symbol or text to search for (case-insensitive substring/symbol match)." :required t)
     (path :type :path :doc "Optional AST path to constrain the search scope. If omitted, searches the entire workspace."))
  (let ((results (perform-search structural-editing-mcp.workspace:*workspace-tree* path query)))
    (if results
        (fmt "Found ~A matches. Paths:~%~{~A~^~%~}" (length results) results)
        (fmt "No matches found for '~A' at path ~A" query path))))

(define-mcp-tool "ast_rename"
    (:description "Rename all occurrences of an identifier/symbol across the workspace or within a specific subtree. Operates strictly on symbol leaf nodes, preserving comments and string literals."
     :mutation t)
    ((old_name :type :string :doc "The exact symbol/string to replace (e.g. 'make-api-call')." :required t)
     (new_name :type :string :doc "The new symbol/string to replace it with (e.g. 'execute-api-call')." :required t)
     (path :type :path :doc "Optional AST path to constrain the bulk rename to a specific subtree. If omitted, renames globally across the workspace."))
  (setf structural-editing-mcp.workspace:*workspace-tree*
        (perform-rename structural-editing-mcp.workspace:*workspace-tree* path old_name new_name))
  (fmt "Successfully renamed all occurrences of '~A' to '~A'." old_name new_name))

(define-mcp-tool "ast_replace_pattern"
    (:description "Search the workspace for a structural Lisp pattern and replace it with a new pattern, preserving matched variables (e.g. pattern='(foo ?x ?y)', replacement='(bar ?y ?x)'). Ideal for semantic API migrations and structural refactorings."
     :mutation t)
    ((pattern :type :string :doc "The pattern to match. Variables start with '?' (e.g. '(make-api-call ?method ?url ?headers ?body)')." :required t)
     (replacement :type :string :doc "The replacement template (e.g. '(make-api-call ?url ?method :headers ?headers :body ?body)')." :required t))
  (setf structural-editing-mcp.workspace:*workspace-tree*
        (structural-editing-mcp.refactor:replace-pattern
         structural-editing-mcp.workspace:*workspace-tree* pattern replacement))
  (fmt "Successfully executed pattern replacement across workspace."))

(define-mcp-tool "ast_extract_variable"
    (:description "Extracts an AST node into a local `let` binding wrapped around its immediate parent. Preserves sub-expression structure and automatically replaces the node with the bound variable."
     :mutation t)
    ((path :type :path :doc "AST path of the node to extract." :required t)
     (variable_name :type :string :doc "The name of the new variable to bind it to." :required t))
  (let ((eff-dialect (or dialect structural-editing-mcp.parser:*current-dialect*)))
    (setf structural-editing-mcp.workspace:*workspace-tree*
          (structural-editing-mcp.refactor:extract-variable
           structural-editing-mcp.workspace:*workspace-tree*
           path variable_name :dialect eff-dialect))
    (fmt "Successfully extracted node at ~A into variable '~A'." path variable_name)))

(define-mcp-tool "ast_extract_function"
    (:description "Extracts an AST node into a new top-level function definition and replaces the original node with a call to the new function. Emits the new function right before the current top-level form."
     :mutation t)
    ((path :type :path :doc "AST path of the node to extract into a function." :required t)
     (function_name :type :string :doc "The name of the new function." :required t)
     (params :type :string-array :doc "Optional list of parameter names for the new function."))
  (let ((eff-dialect (or dialect structural-editing-mcp.parser:*current-dialect*)))
    (setf structural-editing-mcp.workspace:*workspace-tree*
          (structural-editing-mcp.refactor:extract-function
           structural-editing-mcp.workspace:*workspace-tree*
           path function_name :params (to-list params) :dialect eff-dialect))
    (fmt "Successfully extracted node at ~A into function '~A'." path function_name)))

(define-mcp-tool "ast_lint"
    (:description "Run static analysis and structural linting to identify code smells, anti-patterns, and opportunities for refactoring. Returns a list of findings with AST paths, messages, severity, and suggested quick-fixes."
     :mutation nil)
    ((path :type :path :doc "Optional AST path to lint a specific node or file. If omitted, lints the entire workspace.")
     (dialect :type :dialect)
     (rules :type :string-array :doc "Optional list of rule names to run (e.g. ['if-progn-to-when', 'redundant-progn']). If omitted, runs all rules."))
  (run-tool-lint structural-editing-mcp.workspace:*workspace-tree* path dialect args))

(define-mcp-tool "ast_complexity_metrics"
    (:description "Calculate cyclomatic complexity and nesting depth metrics for functions and top-level forms. Identifies candidates for functional decomposition."
     :mutation nil)
    ((path :type :path :doc "Optional AST path to analyze. If omitted, analyzes all functions in the workspace.")
     (dialect :type :dialect)
     (min_complexity :type :integer :doc "Minimum cyclomatic complexity threshold to include in report (default: 1)." :default 1)
     (min_depth :type :integer :doc "Minimum parenthetical nesting depth threshold to include in report (default: 1)." :default 1))
  (run-tool-complexity structural-editing-mcp.workspace:*workspace-tree* path dialect args))

(define-mcp-tool "ast_find_duplicates"
    (:description "Find duplicate code structures and identical subtrees across files in the workspace. Useful for identifying candidate utility functions to extract."
     :mutation nil)
    ((path :type :path :doc "Optional AST path to search within. If omitted, searches across all loaded files.")
     (min_size :type :integer :doc "Minimum node count of duplicate subtree (default: 3)." :default 3)
     (min_occurrences :type :integer :doc "Minimum number of occurrences to report (default: 2)." :default 2))
  (run-tool-duplicates structural-editing-mcp.workspace:*workspace-tree* path args))

(define-mcp-tool "ast_analyze_bindings"
    (:description "Analyze lexical variable bindings, scope chains, unused variables, and variable shadowing across the workspace or within a specific function/node."
     :mutation nil)
    ((path :type :path :doc "Optional AST path to analyze. If omitted, analyzes all files in the workspace.")
     (dialect :type :dialect))
  (run-tool-bindings structural-editing-mcp.workspace:*workspace-tree* path dialect args))

(define-mcp-tool "ast_suggest_refactorings"
    (:description "Aggregate analysis findings from lint, complexity, duplicates, and binding analysis into a prioritized refactoring plan with specific AST paths and actions."
     :mutation nil)
    ((path :type :path :doc "Optional AST path to analyze. If omitted, analyzes the whole workspace.")
     (dialect :type :dialect)
     (min_priority :type :string :doc "Minimum priority threshold to include in plan: 'critical', 'warning', 'suggestion', 'style' (default: 'style')."
                   :enum ("critical" "warning" "suggestion" "style") :default "style"))
  (run-tool-suggestions structural-editing-mcp.workspace:*workspace-tree* path dialect args))

(define-mcp-tool "workspace_create_file"
    (:description "Create a new source file in the staged workspace without writing to disk. The file is registered in memory, staged for AST operations, and persisted only when commit_workspace is called."
     :mutation nil)
    ((path :type :string :doc "File system path of the new file (e.g. '/path/to/src/new-module.lisp').")
     (filepath :type :string :doc "File system path of the new file.")
     (file_path :type :string :doc "File system path of the new file.")
     (content :type :string :doc "Initial file contents (default: empty file)." :default "")
     (dialect :type :dialect))
  (let* ((target-ws (or workspace_id "default"))
         (file-path (or filepath file_path path))
         (ws (structural-editing-mcp.workspace:get-workspace target-ws)))
    (unless file-path
      (error "filepath is required for workspace_create_file"))
    (structural-editing-mcp.workspace:add-file-to-workspace
     file-path
     :content (or content "")
     :dialect dialect
     :ctx ws)
    (fmt "File ~S created successfully in workspace ~S (staged in memory; uncommitted)."
         file-path target-ws)))

(define-mcp-tool "workspace_rebase"
    (:description "Rebase a branch workspace onto its parent workspace, integrating upstream changes. Resolves non-colliding changes automatically and detects collisions."
     :mutation nil)
    ((source_workspace_id :type :string :doc "The branch workspace to rebase (e.g. 'agent-1').")
     (target_workspace_id :type :string :doc "Target workspace to rebase onto (defaults to 'default').")
     (onto_workspace_id :type :string :doc "Target workspace to rebase onto (defaults to parent).")
     (strategy :type :string :doc "Rebase strategy: 'three-way' (default), 'theirs' (prefer upstream on collision), 'ours' (keep branch changes on collision)."
               :enum ("three-way" "theirs" "ours" "error") :default "three-way"))
  (let* ((ws-id (or source_workspace_id workspace_id "default"))
         (onto-id (or target_workspace_id onto_workspace_id "default"))
         (strat (intern (string-upcase (or strategy "three-way")) :keyword))
         (res (structural-editing-mcp.workspace:rebase-workspace ws-id :onto-id onto-id :strategy strat)))
    (if (getf res :conflicts)
        (format-rebase-conflict-summary ws-id (getf res :conflicts))
        (fmt "Workspace ~S successfully rebased onto ~S (~A file(s) updated)."
             ws-id onto-id (length (getf res :updated-files))))))

(define-mcp-tool "workspace_manage"
    (:description "Manage workspace lifecycle: 'list' (all workspaces and revisions), 'create' (new empty workspace), 'delete' (remove workspace), 'clear' (reset workspace), 'fork' (create isolated branch workspace), 'snapshot' (checkpoint workspace revision), 'restore' (rollback to checkpoint), 'create_file' (add empty file in memory), 'rebase' (rebase branch onto parent), 'reload' (reload disk files)."
     :mutation nil)
    ((action :type :string :doc "Lifecycle action."
             :enum ("list" "create" "delete" "clear" "fork" "snapshot" "restore" "create_file" "add_file" "rebase" "reload")
             :default "list")
     (target_id :type :string :doc "Target workspace identifier for create, fork, or rebase.")
     (source_id :type :string :doc "Source workspace identifier for fork (default: 'default')." :default "default")
     (snapshot_name :type :string :doc "Name for snapshot or restore checkpoint." :default "checkpoint")
     (force :type :boolean :doc "Optional bypass flag for clearing modified workspaces or reloading.")
     (files :type :string-array :doc "Optional file list for reload or selective restore."))
  (let* ((act (string-downcase (or action "list")))
         (ws-id (or workspace_id "default"))
         (src (or source_id "default"))
         (tgt (or target_id workspace_id))
         (snap (or snapshot_name "checkpoint"))
         (file-list (to-list files)))
    (string-case act
      ("list" (format-workspaces-list (structural-editing-mcp.workspace:list-workspaces)))
      ("create"
       (unless tgt (error "target_id or workspace_id required for create"))
       (structural-editing-mcp.workspace:create-workspace tgt)
       (fmt "Workspace ~S created successfully." tgt))
      ("delete"
       (structural-editing-mcp.workspace:delete-workspace ws-id)
       (fmt "Workspace ~S deleted successfully." ws-id))
      ("clear"
       (let ((ws (structural-editing-mcp.workspace:get-workspace ws-id)))
         (structural-editing-mcp.workspace:clear-workspace ws :force force)
         (fmt "Workspace ~S cleared successfully." ws-id)))
      ("fork"
       (unless tgt (error "target_id required for fork"))
       (structural-editing-mcp.workspace:fork-workspace src tgt)
       (fmt "Workspace ~S successfully forked into ~S." src tgt))
      (("snapshot" "restore")
       (manage-snapshot-restore act ws-id snap))
      (("create_file" "add_file")
       (manage-create-file args ws-id))
      ("rebase"
       (manage-rebase-workspace args ws-id))
      ("reload"
       (manage-reload-workspace ws-id file-list force))
      (t (error "Unknown workspace_manage action: ~A" act)))))

(define-mcp-tool "workspace_status"
    (:description "Query detailed status of a workspace: dirty/clean files, current/base revisions, snapshots."
     :mutation nil)
    ()
  (let* ((ws-id (or workspace_id "default"))
         (ws (structural-editing-mcp.workspace:get-workspace ws-id))
         (st (structural-editing-mcp.workspace:workspace-status ws)))
    (format-workspace-status-summary st)))

(define-mcp-tool "workspace_diff"
    (:description "Compute AST and file-level difference between two workspaces or against base revision. Highlights disjoint/auto-mergeable files and colliding modifications."
     :mutation nil)
    ((source_workspace_id :type :string :doc "Source workspace identifier.")
     (target_workspace_id :type :string :doc "Target workspace identifier (default: 'default').")
     (other_workspace_id :type :string :doc "The workspace ID to compare against (e.g. 'default' or a feature branch)."))
  (let* ((ws-a (or source_workspace_id workspace_id "default"))
         (ws-b (or target_workspace_id other_workspace_id "default"))
         (diff-plist (structural-editing-mcp.workspace:diff-workspaces ws-a ws-b)))
    (format-workspace-diff-summary diff-plist)))

(define-mcp-tool "workspace_merge"
    (:description "Merge changes from another workspace into the current workspace with AST-level disjoint merge. Safely combines disjoint top-level forms within files without text conflicts."
     :mutation nil)
    ((source_workspace_id :type :string :doc "Source workspace identifier to merge from." :required t)
     (target_workspace_id :type :string :doc "Target workspace identifier receiving changes (default: 'default')." :default "default")
     (files :type :string-array :doc "Optional list of specific files to transfer instead of merging all modified files."))
  (let* ((src-id source_workspace_id)
         (tgt-id (or target_workspace_id workspace_id "default"))
         (file-list (to-list files))
         (res (structural-editing-mcp.workspace:merge-workspaces src-id tgt-id :files file-list)))
    (fmt "Successfully merged workspace ~S into ~S (~A merge, ~A file(s) transferred)."
         src-id tgt-id (getf res :action) (length (getf res :merged-files)))))

(define-mcp-tool "commit_workspace"
    (:description "Persist in-memory workspace modifications to disk files. ONLY modified/dirty files are written. Clean files and unmodified forms are preserved bit-for-bit."
     :mutation nil)
    ((files :type :string-array :doc "Optional list of file paths to persist. If omitted, persists all dirty files in the workspace."))
  (let ((file-list (to-list files)))
    (structural-editing-mcp.workspace:write-workspace file-list)
    (if file-list
        (fmt "Committed ~A specified file(s) to disk successfully." (length file-list))
        (fmt "Workspace committed to disk successfully.~%TIP: Remember to run bash test/verification commands to confirm your changes compile and pass tests!"))))
