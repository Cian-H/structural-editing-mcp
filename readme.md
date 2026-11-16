# Structural Editing MCP

A Model Context Protocol (MCP) server providing AST-level structural editing,
semantic refactoring, multi-branch workspace staging, and static code analysis
for Lisp family languages.

`structural-editing-mcp` allows AI coding assistants (such as Claude, Cursor,
and Antigravity) to inspect, manipulate, and analyze Lisp codebases directly as
Abstract Syntax Trees rather than brittle text strings or line-based diffs. This
eliminates unbalanced parentheses, corrupted reader macros, and fragile
whitespace diffs during automated code modifications.

--------------------------------------------------------------------------------

## Key Features

- **AST-Aware Structural Surgery**: Insert, overwrite, wrap, delete, unwrap,
  promote, move, copy, swap, split, and merge expressions safely at the syntax
  tree level.
- **Multi-Dialect Support**: First-class support for Common Lisp, Clojure,
  Scheme / Racket, Emacs Lisp, and Fennel.
- **Hashnode Handles & Precise Path Addressing**: Hierarchical 0-indexed integer paths
  (`[dialect, file, form, ...]`), symbolic selectors (`"defun:foo"`), and
  hash-verified node handles (`#n<id>:<hash>`) provide unambiguous,
  stale-write-safe coordinates for any expression or atom.
- **Multi-Branch In-Memory Workspace Staging**: Stage multi-file edits in an
  in-memory workspace tree (`*workspace-tree*`). Fork isolated branch workspaces
  for parallel agent workflows, create checkpoints, compute AST diffs, and
  rebase or merge changes with 3-way AST conflict resolution.
- **Context-Preserving Inspection**: Inspect wide or deeply nested code safely
  using skeleton mode (`mode: "skeleton"`) or vertical ray paths (`read_slice`),
  preventing LLM context window blowout.
- **High-Level Refactoring Primitives**: Fast symbol search, semantic bulk
  renaming, structural pattern replacement with wildcards (`?x`, `?y`),
  `let`-binding extraction, and function extraction.
- **Comprehensive Static Analysis**: 19 multi-dialect structural lint rules,
  cyclomatic complexity & nesting depth metrics, code clone detection, lexical
  variable binding analysis (unused & shadowed variables), and a unified
  refactoring advisor.
- **Multi-Platform Standalone Binaries & Containers**: Pre-built binaries for
  Linux (x86_64, aarch64), macOS (Apple Silicon, Intel), and Windows (x86_64),
  plus a minimal Alpine-based Docker container image (~22 MB).

--------------------------------------------------------------------------------

## Supported Dialects

The server automatically detects the dialect from file extensions:

| Dialect             | Extensions                       | Delimiters & Dialect Features                                                   |
| :------------------ | :------------------------------- | :------------------------------------------------------------------------------ |
| **Common Lisp**     | `.lisp`, `.cl`, `.asd`, `.lsp`   | Standard s-expressions, reader literals, package-qualified symbols              |
| **Clojure**         | `.clj`, `.cljs`, `.cljc`, `.edn` | Vectors `[...]`, maps/sets `{...}`, comma as whitespace, keywords `:kw`         |
| **Scheme / Racket** | `.scm`, `.ss`, `.rkt`, `.sld`    | Standard lists, square bracket bindings `[var val]`, boolean literals `#t`/`#f` |
| **Emacs Lisp**      | `.el`                            | Standard Elisp syntax, dynamic & lexical scope conventions                      |
| **Fennel**          | `.fnl`                           | Lisp targeting Lua, tables `[...]`, sequential & associative collections        |

--------------------------------------------------------------------------------

## AST Path Addressing

Nodes in the workspace are referenced using 0-indexed integer paths:

```text
[]                      ; Entire workspace root
 └── [0]                ; First dialect partition (e.g. :common-lisp)
      └── [0, 0]        ; First loaded file in that dialect
           └── [0, 0, 2]     ; Third top-level form in that file
                └── [0, 0, 2, 1] ; Second child element of that form
```

### Example

Given the following top-level form in file `[0, 0]`:

```lisp
(defun add-numbers (a b)
  (+ a b))
```

- `[0, 0, 0]` addresses the entire `defun` form: `(defun add-numbers (a b) (+ a b))`
- `[0, 0, 0, 0]` addresses the symbol `defun`
- `[0, 0, 0, 1]` addresses the function name `add-numbers`
- `[0, 0, 0, 2]` addresses the parameter list `(a b)`
- `[0, 0, 0, 2, 0]` addresses parameter `a`
- `[0, 0, 0, 3]` addresses the body expression `(+ a b)`
- `[0, 0, 0, 3, 1]` addresses variable reference `a` inside the body

### Hashnode Handles & Symbolic Selectors

In addition to integer paths, `read_node` and `read_slice` emit **hashnode handles** in `#n<id>:<hash>` format (e.g. `#n402:3a9f`):

- **`#n<id>`** identifies the specific AST node occurrence (preserved across path reindexing and child edits).
- **`:<hash>`** is a 4-character FNV-1a structural hash of the subtree. Passing `#n402:3a9f` to any tool verifies that the node has not changed since your last read; if it has, the server aborts with a `hash_mismatch` error and returns a fresh preview with the updated handle.
- Bare `#n<id>` handles (without `:<hash>`) and symbolic segment lists (e.g. `["main.lisp", "defun:add-numbers", 3]`) are also supported.

--------------------------------------------------------------------------------

## Tool Reference

The MCP server exposes 22 specialized tools across five functional tiers:

### 1. Workspace Inspection & Navigation

| Tool | Parameters | Description |
| :--- | :--- | :--- |
| `read_node` | `path` *(optional)*<br>`depth` *(default: 2)*<br>`mode` *(default: "auto")*<br>`limit` *(default: 50)*<br>`offset` *(default: 0)*<br>`load_files` | Inspects an AST node or the entire workspace. Returns formatted source code and child paths up to `depth`. Supports `mode: "skeleton"` (and auto-truncation) for compact structural metadata stubs on wide trees. Pass `load_files` on initial call to populate the workspace from disk. |
| `read_slice` | `path` *(required)*<br>`depth` *(default: 2)*<br>`mode` *(default: "full")*<br>`limit` *(default: 50)*<br>`offset` *(default: 0)*<br>`load_files` | Casts a vertical ray/spine down to a target node, strictly eliding lateral siblings at ancestor levels while fully hydrating the target node. Eliminates context window blowout when inspecting deeply nested nodes in wide trees. |

### 2. Workspace Staging, Branching & Multi-Agent Lifecycle

| Tool | Parameters | Description |
| :--- | :--- | :--- |
| `workspace_create_file` | `path` / `filepath` *(required)*<br>`content` *(optional)*<br>`dialect` *(optional)*<br>`workspace_id` *(optional)* | Creates a new source file staged purely in memory without touching disk. Persisted only when `commit_workspace` is called. |
| `workspace_manage` | `action` *(default: "list")*<br>`target_id`<br>`source_id` *(default: "default")*<br>`snapshot_name`<br>`force`<br>`files`<br>`workspace_id` | Manages workspace lifecycle: `"list"` (active workspaces & revisions), `"create"`, `"delete"`, `"clear"`, `"fork"` (isolated branch workspace for parallel agent tasks), `"snapshot"` (checkpoint), `"restore"` (rollback), `"create_file"`, `"rebase"`, and `"reload"`. |
| `workspace_status` | `workspace_id` *(optional)* | Queries detailed status of a workspace: clean/dirty files, current revision count, base revision, and checkpoints. |
| `workspace_diff` | `source_workspace_id`<br>`target_workspace_id` *(default: "default")*<br>`workspace_id` | Computes AST- and file-level diff between two workspaces or against a base revision. Highlights disjoint/auto-mergeable files and colliding modifications. |
| `workspace_rebase` | `source_workspace_id`<br>`target_workspace_id` *(default: "default")*<br>`strategy` *(default: "three-way")* | Rebases a branch workspace onto upstream, automatically integrating non-colliding AST changes using 3-way merge, `"theirs"`, or `"ours"` strategies. |
| `workspace_merge` | `source_workspace_id` *(required)*<br>`target_workspace_id` *(default: "default")*<br>`files` *(optional)* | Merges changes from a branch workspace into target with AST-level disjoint merge across and within files. |
| `commit_workspace` | `files` *(optional)* | Persists modified staged files from in-memory workspace to disk. Only dirty files are written; clean files remain untouched bit-for-bit. |

### 3. Structural Editing (AST Surgery)

All mutation tools automatically return an updated structural preview of the enclosing parent node, eliminating redundant verification reads. Pass a hashnode handle (`#n<id>:<hash>`) for per-node stale-write protection, and optionally `expected_revision` for workspace-level optimistic concurrency control.

| Tool | Parameters | Description |
| :--- | :--- | :--- |
| `ast_modify` | `path` *(required)*<br>`action`: `"insert"` \| `"overwrite"` \| `"wrap"` *(required)*<br>`new_node` *(required)*<br>`index` *(optional)*<br>`end_index` *(optional)*<br>`force` *(optional)* | **`insert`**: Adds `new_node` before `path` (or at child `index`).<br>**`overwrite`**: Replaces the node at `path` with `new_node` (guarded against overwriting >50 children unless `force: true`).<br>**`wrap`**: Encloses the node (or child range `index` to `end_index`) in `:paren`, `:square`, `:curly`, or an enclosing form string (e.g. `(when valid-p)`). |
| `ast_remove` | `path` *(required)*<br>`action`: `"delete"` \| `"unwrap"` \| `"promote"` *(required)*<br>`new_node` *(optional)* | **`delete`**: Removes the node at `path`.<br>**`unwrap`**: Strips the enclosing collection, spilling children into the parent.<br>**`promote`**: Replaces the parent node with the child at `path`. |
| `ast_relocate` | `action`: `"move"` \| `"copy"` \| `"swap"` \| `"merge"` \| `"split"` *(required)*<br>`source_path`<br>`target_path`<br>`index` / `target_index`<br>`split_index`<br>`separator` | **`move`** / **`copy`**: Moves or duplicates `source_path` to `target_path` at `index`.<br>**`swap`**: Exchanges the nodes at `source_path` and `target_path`.<br>**`merge`**: Joins two adjacent collections into one.<br>**`split`**: Divides a collection into two at child `split_index`. |

### 4. Semantic Refactoring & Search

| Tool | Parameters | Description |
| :--- | :--- | :--- |
| `ast_search` | `query` *(required)*<br>`path` *(optional)* | Fast search for symbol names, function identifiers, or tokens across the workspace or within a specific subtree. Returns exact AST paths. |
| `ast_rename` | `old_name` *(required)*<br>`new_name` *(required)*<br>`path` *(optional)* | Bulk renames a symbol identifier across the workspace or scoped under a specific subtree while preserving comments and string literals. |
| `ast_replace_pattern` | `pattern` *(required)*<br>`replacement` *(required)* | Structural pattern matching and templated replacement with wildcards (e.g. pattern `(foo ?x ?y)` to replacement `(bar ?y ?x)`). |
| `ast_extract_variable` | `path` *(required)*<br>`variable_name` *(required)*<br>`dialect` *(optional)* | Extracts the sub-expression at `path` into a local `let` binding wrapped around its immediate parent form. |
| `ast_extract_function` | `path` *(required)*<br>`function_name` *(required)*<br>`params` *(optional)*<br>`dialect` *(optional)* | Extracts the sub-expression at `path` into a new top-level function definition, replacing the original site with a call. |

### 5. Static Analysis & Code Quality

| Tool | Parameters | Description |
| :--- | :--- | :--- |
| `ast_lint` | `path` *(optional)*<br>`dialect` *(optional)*<br>`rules` *(optional)* | Structural linter running 19 anti-pattern and safety rules across Common Lisp, Clojure, Emacs Lisp, and Scheme. Returns findings with AST paths, severities, and suggested quick-fixes. |
| `ast_complexity_metrics` | `path` *(optional)*<br>`dialect` *(optional)*<br>`min_complexity` *(default: 1)*<br>`min_depth` *(default: 1)* | Computes cyclomatic complexity, maximum nesting depth, and AST node counts for functions and top-level forms. |
| `ast_find_duplicates` | `path` *(optional)*<br>`min_size` *(default: 3)*<br>`min_occurrences` *(default: 2)* | Detects duplicate AST subtrees and code clones across files to identify helper function extraction targets. |
| `ast_analyze_bindings` | `path` *(optional)*<br>`dialect` *(optional)* | Lexical scope analyzer that flags unused variables and shadowed bindings across function arguments, `let`, `let*`, `flet`, `labels`, `lambda`, and loops. |
| `ast_suggest_refactorings` | `path` *(optional)*<br>`dialect` *(optional)*<br>`min_priority` *(default: "style")* | Multi-engine refactoring advisor that aggregates and prioritizes findings across linting, complexity, duplicates, and bindings into an actionable plan. |

--------------------------------------------------------------------------------

## Static Analysis Rules

The structural linter (`ast_lint`) includes 19 rules designed to catch syntax inefficiencies, ANSI Common Lisp undefined behavior, macro hygiene defects, and dialect-specific runtime traps:

| Rule Name | Dialects | Description |
| :--- | :--- | :--- |
| `if-progn-to-when` | CL, Elisp, Scheme | Suggests `when` for single-branch `(if cond (progn ...))`. |
| `if-nil-to-when` | CL, Elisp, Scheme, Clojure | Suggests `when` for `(if cond then nil)`. |
| `if-not-to-unless` | CL, Elisp, Clojure | Suggests `unless` for `(if (not cond) ...)`. |
| `invert-if-not` | All | Reverses branches of `(if (not cond) else then)` to simplify logic. |
| `single-clause-cond` | CL, Elisp, Scheme, Clojure | Simplifies `(cond (test body))` to `(when test body)`. |
| `if-boolean-redundant` | All | Simplifies `(if cond t nil)` to boolean value or condition. |
| `redundant-progn` | CL, Elisp | Removes `progn` forms occurring in bodies with implicit progn semantics. |
| `nested-let` | CL, Elisp, Scheme | Flags nested `let` forms that can be collapsed into a single `let*`. |
| `equal-nil-to-null` | CL, Elisp | Simplifies `(equal x nil)` to `(null x)`. |
| `ignored-destructive-return` | CL, Elisp | Detects discarded return values from non-guaranteed in-place sequence modifiers (`sort`, `delete`, `nreverse`). |
| `unhygienic-macro-binding` | CL, Elisp, Scheme | Detects literal symbol bindings inside backquoted `let` in `defmacro` (variable capture hazard). |
| `clojure-tail-recur` | Clojure | Identifies recursive self-calls in tail position that should use `recur` to avoid stack blowout. |
| `mutate-literal-constant` | CL, Scheme, Clojure | Catches mutation (`nconc`, `setf`, `sort`) on quoted or literal constants (ANSI CL §3.7.1 violation). |
| `special-var-earmuffs` | CL, Elisp | Flags `defvar` / `defparameter` names missing standard `*...*` earmuffs. |
| `clojure-swap-side-effects` | Clojure | Warns against side-effects or I/O inside STM/CAS retry functions (`swap!`, `alter`, `dosync`). |
| `dead-cond-clauses` | All | Detects dead clauses positioned after an unconditional default clause (`t` or `otherwise`) in `cond`. |
| `inappropriate-equality` | CL, Scheme | Flags pointer equality (`eq` / `eq?`) used to compare numbers, strings, or characters. |
| `clojure-vector-contains` | Clojure | Flags `contains?` called on vector literals (which tests index presence, not value membership). |
| `elisp-missing-lexical-binding` | Elisp | Flags Emacs Lisp files lacking `;; -*- lexical-binding: t; -*-`. |

--------------------------------------------------------------------------------

## Command-Line Interface (`semcp`)

In addition to running as a stdio MCP server (the default when invoked with no arguments or `semcp mcp`), `semcp` provides a full CLI that mirrors all 22 MCP tools and extends them for shell scripts, CI pipelines, and interactive terminal workflows.

### Execution & Persistence Modes

1. **One-Shot Mode (Default without `.semcp/`)**:
   Pass file or directory paths directly. Read and analysis commands load them ephemerally; mutations preview in memory unless `-i` / `--commit` is passed to write changes to disk immediately.
   ```bash
   semcp read src/main.lisp
   semcp lint src/ --fail-on warning
   semcp modify overwrite 0,0,0,1 new-name -L app.lisp -i
   ```

2. **Staged Session Mode (`.semcp/state.json`)**:
   Initialize a persistent workspace session with `semcp init`. Subsequent commands auto-discover `.semcp/`, stage AST edits and forked branches in memory across CLI calls, and only touch disk when you run `semcp commit`.
   ```bash
   semcp init src/
   semcp ws fork feat-1
   semcp modify overwrite 0,0,2,1 new-fn -w feat-1
   semcp diff feat-1 default
   semcp merge feat-1 --into default
   semcp commit
   ```

3. **In-RAM Daemon Mode (`.semcp/daemon.sock`)**:
   For sub-millisecond execution without re-parsing or re-serializing `state.json` on every command, start the background daemon to hold `*workspace-registry*` in RAM over a Unix domain socket:
   ```bash
   semcp daemon start src/       # Start background daemon (loads .semcp/state.json if present)
   semcp daemon status           # Inspect PID, socket, and workspace/file counts
   semcp modify ...              # Automatically forwarded to the live in-RAM daemon
   semcp daemon save             # Flush in-RAM state to .semcp/state.json
   semcp daemon stop             # Save state and stop the daemon
   ```
   Pass `--no-daemon` to bypass a running daemon for a single command.

### CLI Subcommands & Extensions

- **Inspection**: `read` (`read_node`), `slice` (`read_slice`), `search` (`ast_search`)
- **AST Surgery & Refactoring**: `modify` (`ast_modify`), `remove` (`ast_remove`, plus `delete`/`unwrap`/`promote` shortcuts), `relocate` (`ast_relocate`), `rename` (`ast_rename`), `replace-pattern` (`ast_replace_pattern`), `extract-var` (`ast_extract_variable`), `extract-fn` (`ast_extract_function`) — all support `--stdin` and `-i` / `--commit`
- **Static Analysis**: `lint` (`ast_lint`, with `--fail-on <error|warning|any>` exit code `2`), `complexity`, `duplicates`, `bindings`, `suggest`
- **Workspaces**: `init`, `load`, `ws` / `workspace` (`workspace_manage`), `create-file`, `status`, `diff`, `rebase`, `merge`, `commit`
- **Utilities**: `call <tool> [json|--stdin]` (raw MCP tool invocation), `tools [--json]`, `completions <bash|zsh|fish>`, `semcp <cmd> --help`
- **Machine Output**: Pass `--json` on any command for structured JSON output.

--------------------------------------------------------------------------------

## Installation & Pre-Built Artifacts

### 1. Download Pre-Built Binaries

On every release, pre-compiled standalone executables are published to the
[GitHub Releases](https://github.com/Cian-H/structural-editing-mcp/releases) page:

- **Linux x86_64**: `semcp-linux-x86_64`
- **Linux AArch64**: `semcp-linux-aarch64`
- **macOS Apple Silicon**: `semcp-darwin-arm64`
- **macOS Intel**: `semcp-darwin-x86_64`
- **Windows x86_64**: `semcp-windows-x86_64.exe`

### 2. Multi-Arch Docker Container Image

Lightweight container images (~22 MB runtime based on Alpine Linux) supporting
both `linux/amd64` and `linux/arm64` are available via GitHub Container Registry:

```bash
docker pull ghcr.io/cian-h/structural-editing-mcp:latest
```

Run via stdio:

```bash
docker run -i --rm ghcr.io/cian-h/structural-editing-mcp:latest
```

### 3. Build from Source

#### Prerequisites
- [SBCL](https://www.sbcl.org/) (Steel Bank Common Lisp)
- [devenv](https://devenv.sh/) (recommended for reproducible Nix builds) or Quicklisp
- Dependencies: `trivia`, `alexandria`, `serapeum`, `yason`, `cl-indentify`, `bordeaux-threads`, `rove`

#### Build Binary
```bash
./scripts/build.lisp
# Produces standalone executable: ./semcp
```

--------------------------------------------------------------------------------

## Client Configuration

Configure your AI assistant or editor MCP client to communicate with `structural-editing-mcp` over standard I/O:

### Standalone Executable (Recommended)

Add to your MCP configuration (`claude_desktop_config.json`, `.cursor/mcp.json`, or Antigravity settings):

```json
{
  "mcpServers": {
    "structural-editing": {
      "command": "/path/to/semcp"
    }
  }
}
```

### Docker Container

```json
{
  "mcpServers": {
    "structural-editing": {
      "command": "docker",
      "args": [
        "run",
        "-i",
        "--rm",
        "-v", "/path/to/your/project:/workspace",
        "ghcr.io/cian-h/structural-editing-mcp:latest"
      ]
    }
  }
}
```

### via `devenv` / SBCL Source

```json
{
  "mcpServers": {
    "structural-editing": {
      "command": "devenv",
      "args": [
        "shell",
        "--",
        "sbcl",
        "--noinform",
        "--noprint",
        "--disable-debugger",
        "--script",
        "/path/to/structural-editing-mcp/scripts/run-server.lisp"
      ]
    }
  }
}
```

--------------------------------------------------------------------------------

## Development & Testing

### Running the Stdio Server Directly
```bash
./scripts/run-server.lisp
```

### Running Test Suite
Run all unit and integration tests using [Rove](https://github.com/fukamachi/rove):
```bash
./scripts/run-tests.lisp
```

To run a specific test suite or test:
```bash
./scripts/run-tests.lisp parser-tests
```

### Code Formatting
Format Common Lisp code using AST-aware indentation rules:
```bash
./scripts/format.lisp
```

### Versioning (CalVer)
The project adheres to Calendar Versioning (`YYYY.M.D.N`).
```bash
./scripts/calver.lisp --print   # Print current version
./scripts/calver.lisp --check   # Check version consistency
./scripts/calver.lisp --next    # Compute next version
./scripts/calver.lisp --update  # Update version.txt and git tag
```

A pre-push hook (`.githooks/pre-push`) automatically updates and tags the version before pushing.

--------------------------------------------------------------------------------

## Project Architecture

```text
structural-editing-mcp/
├── scripts/
│   ├── build.lisp          # Standalone binary compiler (sb-ext:save-lisp-and-die)
│   ├── calver.lisp         # Calendar versioning CLI and tag management
│   ├── format.lisp         # AST-aware code indentation and layout formatter
│   ├── run-server.lisp     # Stdio JSON-RPC MCP server runner
│   └── run-tests.lisp      # Rove test suite runner
├── src/
│   ├── analysis/           # Modular static code analysis subsystem
│   │   ├── bindings.lisp   # Lexical variable binding and scope tracker
│   │   ├── common.lisp     # AST walking utilities and threshold evaluation
│   │   ├── complexity.lisp # Cyclomatic complexity and nesting depth metrics
│   │   ├── duplicates.lisp # Subtree clone and duplicate code detection
│   │   ├── lint.lisp       # 19 structural lint rules and diagnostic reporter
│   │   ├── package.lisp    # Analysis package definition and exports
│   │   ├── patterns.lisp   # Structural pattern matching utilities
│   │   └── suggestions.lisp# Unified refactoring plan advisor
│   ├── cli/                # Command-line interface & in-RAM daemon subsystem
│   │   ├── args.lisp       # Flag, argument, and AST path parser
│   │   ├── cli.lisp        # Top-level CLI dispatcher, help text, JSON output
│   │   ├── commands.lisp   # Subcommand handlers mirroring & extending MCP tools
│   │   ├── daemon.lisp     # Unix-socket in-RAM workspace daemon
│   │   ├── package.lisp    # CLI package definition and exports
│   │   └── session.lisp    # Persistent .semcp/state.json serialization
│   ├── mcp/                # Model Context Protocol subsystem
│   │   ├── core.lisp       # define-mcp-tool macro and tool dispatch registry
│   │   ├── package.lisp    # MCP package definition and exports
│   │   ├── preview.lisp    # Node previews, skeleton rendering, vertical slice rays
│   │   ├── protocol.lisp   # JSON-RPC 2.0 serialization and protocol loop
│   │   ├── server.lisp     # Stdio server loop and signal handling
│   │   └── tools.lisp      # Unified tool declarations and handlers (22 tools)
│   ├── conditions.lisp     # Custom condition hierarchy and error definitions
│   ├── edit.lisp           # Functional AST surgery primitives (insert, wrap, move, etc.)
│   ├── main.lisp           # CLI entry point, version/help flags, signal handling
│   ├── parser.lisp         # Multi-dialect tokenizer, reader, printer, trivia matchers
│   ├── refactor.lisp       # Pattern replacement, let-binding and function extraction
│   ├── tree.lisp           # AST node representations, path indexing, tree traversal
│   ├── version.lisp        # CalVer version resolution
│   └── workspace.lisp      # In-memory staging, isolated branches, 3-way AST merge, disk I/O
├── test/                   # Comprehensive Rove test suites
├── Dockerfile              # Multi-stage Alpine container build (~22 MB runtime)
├── devenv.nix              # Reproducible Nix environment specification
├── structural-editing-mcp.asd # ASDF system definition
├── version.txt             # Current CalVer version
└── license.md              # GNU Lesser General Public License v3.0
```

--------------------------------------------------------------------------------

## License

Distributed under the terms of the GNU Lesser General Public License v3.0 (LGPLv3).
See [license.md](license.md) for details.
