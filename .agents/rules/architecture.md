# Architecture & AST Data Structures

> In-depth reference for AI agents working on the AST representations, workspace hierarchy, and MCP server architecture of `structural-editing-mcp`.

---

## 1. Abstract Syntax Tree (AST) Model

All Lisp code is parsed into a uniform, tagged functional tree structure defined in [`src/tree.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/tree.lisp).

### AST Node Types

There are three primary node shapes matched using `trivia:match`:

1. **Compound Node (`node`)**:
   ```lisp
   (:path <path-list> <tag> <children-list>)
   ```
   - Pattern: `(structural-editing-mcp.tree:node path tag children)`
   - Tags include:
     - `:workspace`: The root of the entire multi-file project workspace.
     - Dialect tags: `:common-lisp`, `:clojure`, `:scheme`, `:emacs-lisp`, `:fennel`.
     - `:file`: Represents a source file loaded into the workspace.
     - `:paren`: Standard parentheses `(...)`.
     - `:square`: Square brackets `[...]` (Clojure vectors, Scheme let-bindings).
     - `:curly`: Curly braces `{...}` (Clojure maps and sets).

2. **Leaf Node (`leaf`)**:
   ```lisp
   (:path <path-list> :leaf <value>)
   ```
   - Pattern: `(structural-editing-mcp.tree:leaf path val)`
   - `val` is an atom: a symbol name string, number, string literal, keyword, or character literal.

3. **Comment / Trivia Node (`comment`)**:
   ```lisp
   (:path <path-list> :comment <text>)
   ```
   - Pattern: `(structural-editing-mcp.tree:comment path text)`
   - Contains single-line `; ...` or multi-line `#| ... |#` comment text. Preserving these is essential for non-destructive refactoring.

---

## 2. Integer Path Addressing Coordinates

Nodes are addressed using 0-indexed integer lists. Each element in the list represents an index into the parent node's children list.

### Workspace Coordinate Hierarchy

```text
()                                  ; [Root] Workspace node (:workspace)
 ├── (0)                            ; [Dialect 0] e.g. :common-lisp
 │    ├── (0 0)                     ; [File 0 in Dialect 0] e.g. src/parser.lisp
 │    │    ├── (0 0 0)              ; [Form 0] First top-level form (e.g. defpackage)
 │    │    └── (0 0 1)              ; [Form 1] Second top-level form (e.g. in-package)
 │    └── (0 1)                     ; [File 1 in Dialect 0] e.g. src/tree.lisp
 └── (1)                            ; [Dialect 1] e.g. :clojure
      └── (1 0)                     ; [File 0 in Dialect 1] e.g. core.clj
```

### Expression Addressing Example

Given the Common Lisp form:
```lisp
(defun square (x)
  (* x x))
```
- Path `(0 0 5)`: The entire `defun` form.
- Path `(0 0 5 0)`: The symbol `defun`.
- Path `(0 0 5 1)`: The function name symbol `square`.
- Path `(0 0 5 2)`: The parameter list `(x)`.
- Path `(0 0 5 2 0)`: The parameter symbol `x`.
- Path `(0 0 5 3)`: The body expression `(* x x)`.
- Path `(0 0 5 3 0)`: The operator `*`.
- Path `(0 0 5 3 1)`: The first argument `x`.
- Path `(0 0 5 3 2)`: The second argument `x`.

### Path Reindexing
Whenever an edit modifies the tree structure (adding, removing, or relocating nodes), sibling and child paths become outdated. Always call:
```lisp
(structural-editing-mcp.tree:reindex-paths node base-path)
```
to recalculate consistent coordinate paths.

---

## 3. Workspace Staging, Multi-Agent Registry, & Concurrency

Defined in [`src/workspace.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/workspace.lisp):

- `*workspace-tree*`: Holds the active AST root for the dynamically bound workspace context.
- `*workspace-registry*`: Hash table mapping workspace IDs (default `"default"`) to `workspace-context` structures.
- `workspace-context`: Encapsulates an isolated project tree (`tree`), parent workspace lineage (`parent-id`), base revision (`base-revision`), current revision counter (`revision`), in-memory snapshots table (`snapshots`), clean state snapshots (`clean-state`), and a dedicated per-workspace mutex lock (`lock`).
- `*file-registry*`: Hash table mapping numerical file IDs and path tuples to physical filesystem paths.
- `load-into-workspace`: Loads a file or scans a directory recursively, classifying dialect by file extension:
  - Common Lisp: `.lisp`, `.cl`, `.asd`, `.lsp`
  - Clojure: `.clj`, `.cljs`, `.cljc`, `.edn`
  - Scheme: `.scm`, `.ss`, `.rkt`, `.sld`
  - Emacs Lisp: `.el`
  - Fennel: `.fnl`
- `write-workspace`: Writes modified AST files back to disk by unparsing the trees with dialect-appropriate pretty printing. Supports selective file committing via `:files`.

### Multi-Agent Workspace Branching & Merging Workflow

1. **Forking**: An agent forks an isolated workspace:
   `workspace_manage(action: "fork", source_id: "default", target_id: "worker-1")`
2. **Editing**: The agent runs AST transformations targeting `workspace_id: "worker-1"`. The mutations stage strictly in memory.
3. **Diffing**: Before merging, check for disjoint changes or collisions:
   `workspace_diff(source_workspace_id: "worker-1", target_workspace_id: "default")`
4. **Merging**:
   `workspace_merge(source_workspace_id: "worker-1", target_workspace_id: "default")`
   - If only the fork has changed, a clean fast-forward occurs.
   - If both workspaces have changed on disjoint files, files are non-destructively grafted.
   - If both modified the same file, a `workspace-merge-conflict-error` is signaled unless specific non-colliding files are explicitly passed in `files`.
5. **Committing**:
   `commit_workspace(workspace_id: "default")` persists in-memory modifications to disk.

---

## 4. MCP Server & JSON-RPC Protocol

Defined in [`src/mcp.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/mcp.lisp):

- **Transport**: JSON-RPC 2.0 over standard I/O (stdio).
- **Initialization Handshake**: Responds to `initialize` and `notifications/initialized`.
- **Tools Discovery**: Responds to `tools/list` with JSON schema metadata for all 22 structural editing, refactoring, analysis, and multi-workspace lifecycle/merging tools:
  - **Inspection**: `read_node` (supports `mode: "skeleton"`), `read_slice` (vertical spine rays)
  - **Structural Surgery**: `ast_modify`, `ast_remove`, `ast_relocate`
  - **Search & Pattern Replacement**: `ast_search`, `ast_rename`, `ast_replace_pattern`
  - **Refactoring & Extraction**: `ast_extract_variable`, `ast_extract_function`, `ast_suggest_refactorings`
  - **Static Analysis**: `ast_lint`, `ast_complexity_metrics`, `ast_find_duplicates`, `ast_analyze_bindings`
  - **Workspace & Multi-Agent Collaboration**: `commit_workspace`, `workspace_create_file`, `workspace_manage`, `workspace_rebase`, `workspace_status`, `workspace_diff`, `workspace_merge`
- **Tool Execution**: Dispatches `tools/call` requests to corresponding Lisp handlers, routing transparently to target workspaces via optional `workspace_id`.
- **Preview Mechanism**: Mutation tools generate an immediate structural code snippet preview of the modified node and its parent, allowing AI agents to visually verify AST modifications before committing.
