# AGENTS.md

> Universal agent instructions and repository guide for `structural-editing-mcp`.
> Followed by all AI coding agents (Antigravity, Cursor, Claude Code, Windsurf, Copilot, Aider, OpenHands, etc.).

---

## 1. Project Overview & Mission

`structural-editing-mcp` is a Model Context Protocol (MCP) server written in Common Lisp (SBCL) providing AST-level structural editing, semantic refactoring, and static code analysis for Lisp-family languages.

### Why Structural Editing Over Text Diffs?
AI code editing on Lisp codebases often fails due to unbalanced parentheses, corrupted reader macros, and fragile whitespace diffs. This project provides:
1. **Tree-Structured Manipulations**: Mutations (`insert`, `wrap`, `overwrite`, `unwrap`, `promote`, `move`, `swap`, `split`, `merge`) operate strictly on AST nodes.
2. **Multi-Dialect Support**: Common Lisp (`.lisp`, `.cl`, `.asd`), Clojure (`.clj`, `.cljs`, `.edn`), Scheme/Racket (`.scm`, `.rkt`), Emacs Lisp (`.el`), and Fennel (`.fnl`).
3. **In-Memory Multi-Workspace Staging**: Modifications stage safely in memory (`*workspace-tree*`) with instant previews, isolated parallel agent branches (`*workspace-registry*`), fast diffing/merging, and are persisted to disk only when explicitly committed via `commit_workspace`.
4. **Static Analysis & Refactoring**: Linting, cyclomatic complexity metrics, code duplicate detection, lexical binding/shadowing analysis, and pattern replacement.

---

## 2. Environment & Prerequisites

The project uses [devenv](https://devenv.sh/) with Nix for hermetic, reproducible dependencies.

- **Primary Runtime**: SBCL (Steel Bank Common Lisp)
- **Key Common Lisp Libraries**:
  - `trivia`: Pattern matching on AST nodes
  - `alexandria` & `serapeum`: Standard utility libraries
  - `yason`: JSON parsing and serialization for MCP JSON-RPC
  - `cl-indentify`: Code layout and indentation formatting
  - `bordeaux-threads`: Cross-platform multithreading and lock management
  - `rove`: Testing framework
- **Environment Entry**:
  - You can run any command in the environment using `devenv shell -- <command>` or run scripts directly via their shebangs (e.g., `./scripts/run-tests.lisp`).

---

## 3. Essential Commands Cheatsheet

Always use these commands for development and verification:

| Task | Command | Notes |
| :--- | :--- | :--- |
| **Run All Tests** | `./scripts/run-tests.lisp` | Executes full Rove test suite. Must exit 0. |
| **Run Targeted Test** | `./scripts/run-tests.lisp <test-symbol>` | e.g. `./scripts/run-tests.lisp test-mcp-tools-list` |
| **Compile Standalone Binary** | `./scripts/build.lisp` | Builds `semcp` via `save-lisp-and-die`. |
| **Run MCP Server via Stdio** | `./scripts/run-server.lisp` | Runs stdio JSON-RPC server with load messages routed to stderr. |
| **Interactive SBCL Shell** | `devenv shell -- sbcl` | Starts SBCL REPL with all dependencies loaded. |

---

## 4. Codebase Architecture & File Map

```text
structural-editing-mcp/
├── AGENTS.md                  # This file: universal agent documentation & invariants
├── structural-editing-mcp.asd # ASDF system definitions (core and tests)
├── devenv.nix                 # Nix environment and dependency declarations
├── scripts/
│   ├── build.lisp             # Standalone binary compiler
│   ├── run-server.lisp        # JSON-RPC stdio server runner
│   └── run-tests.lisp         # Rove test runner with CLI argument support
├── src/
│   ├── conditions.lisp        # Custom condition hierarchy (mcp-error, invalid-path-error, etc.)
│   ├── utils.lisp             # General list/string manipulation, dictionary/JSON helpers
│   ├── tree.lisp              # AST representations, trivia pattern matching, path indexing
│   ├── parser.lisp            # Multi-dialect tokenizers, tree builder, pretty-printer
│   ├── edit.lisp              # Functional AST surgery (ast-modify, ast-remove, ast-relocate)
│   ├── analysis.lisp          # Linter, cyclomatic complexity, clone finder, binding tracker
│   ├── refactor.lisp          # Structural pattern replacement, let & defun extraction
│   ├── workspace.lisp         # Workspace tree staging (*workspace-tree*), file registry, disk I/O
│   ├── mcp.lisp               # JSON-RPC protocol parser, MCP tool registry & dispatchers
│   └── main.lisp              # Standalone binary entry point and signal handling
├── test/
│   ├── package.lisp           # Test package definition and test helpers
│   ├── conditions.lisp        # Tests for error handling and conditions
│   ├── tree.lisp              # Tests for AST path traversal and pattern matching
│   ├── parser.lisp            # Tests for multi-dialect lexing, parsing, and printing
│   ├── edit.lisp              # Tests for AST surgery primitives
│   ├── analysis.lisp          # Tests for linting, complexity, duplicates, and binding analysis
│   ├── workspace.lisp         # Tests for multi-dialect workspace loading and disk commit
│   └── mcp.lisp               # Tests for MCP JSON-RPC protocol and tool endpoints
└── .agents/
    ├── mcp_config.json        # Standard MCP client configuration for local agent usage
    ├── rules/                 # In-depth architectural and coding standard references
    └── skills/                # Task-specific agent skills (testing, AST surgery, MCP tools)
```

---

## 5. Non-Negotiable Invariants for AI Agents

When modifying or extending this codebase, adhere strictly to these rules:

### 1. Stdio Purity on the MCP Server
- **NEVER print diagnostic or debug text to `*standard-output*`!**
- The MCP server communicates strictly via JSON-RPC 2.0 over standard I/O. Any unexpected character on `*standard-output*` (such as `format t`, `print`, ASDF compilation notes, or warnings) will corrupt the JSON stream and crash client connections.
- All diagnostics, logs, or error messages MUST be written to `*error-output*` (e.g. `(format *error-output* "~&Log: ...~%")`).

### 2. In-Memory Workspace Staging
- Never write ad-hoc file mutations directly to disk during AST operations.
- The workflow must always be:
  1. Load into `*workspace-tree*` (via `load-into-workspace`).
  2. Perform AST transformations in memory.
  3. Re-index node paths with `reindex-paths`.
  4. Write back to disk only when `write-workspace` / `commit_workspace` is invoked.

### 3. AST Path Coordinate Integrity
- Nodes are addressed by 0-indexed integer lists representing the descent through the workspace tree:
  - `[]`: Workspace root
  - `[dialect_idx]`: Dialect partition (e.g., `0` for Common Lisp)
  - `[dialect_idx, file_idx]`: File node
  - `[dialect_idx, file_idx, form_idx, ...]`: Expressions and sub-expressions
- Whenever the shape of an AST node changes (insertion, deletion, relocation, extraction), paths in that subtree must remain synchronized via `reindex-paths`.

### 4. Headless Error Handling & SBCL Debugger
- Headless agents will freeze if SBCL drops into an interactive debugger prompt on an unhandled error.
- All server and test runners must run with `--disable-debugger`.
- Signal specific condition types defined in `src/conditions.lisp`.
- Handle expected errors in `src/mcp.lisp` and return standard JSON-RPC error packets (`send-error`) rather than letting conditions propagate to the top level.

### 5. Multi-Dialect Syntax Preservation
- When implementing transformations or parsing rules, respect the dialect:
  - Common Lisp uses `(...)` and package symbols.
  - Clojure uses `[...]` for vectors and `{...}` for maps.
  - Scheme uses `[...]` in binding constructs like `let`.
  - Comments and trivia must be preserved during round-trip parsing and unparsing.

### 6. Verification Discipline
- Before finishing any task or making a commit, always run:
  ```bash
  ./scripts/run-tests.lisp
  ```
- All unit and integration tests must pass with zero failures.

### 7. Dogfooding: Prefer Structural Editing MCP for Lisp Code
- When the `structural-editing` MCP tools (or server) are available in your agent environment, you **MUST prioritize using them** over standard text-editing tools (`replace_file_content`, text diffs, ad-hoc file writes) for reading, modifying, refactoring, and persisting all Lisp code (`.lisp`, `.cl`, `.asd`).
- **Primary Toolchain**:
  - Inspection: `read_node` (with `load_files: [...]`), `ast_search`
  - Mutation: `ast_modify`, `ast_remove`, `ast_relocate`
  - Refactoring: `ast_rename`, `ast_replace_pattern`, `ast_extract_variable`, `ast_extract_function`
  - Persistence: `commit_workspace`
- **Forbidden on Lisp Code**: Do NOT use text-based replacement or diff tools on Lisp source files when the MCP server is operational. Editing Lisp via raw text diffs violates the core philosophy of this project and risks parenthesis imbalance.
- **Allowed Exceptions & Fallbacks**:
  - *Non-Lisp Files*: Always use standard text tools (`replace_file_content`, `write_to_file`) for documentation (`.md`), configurations (`.json`, `.nix`), and shell scripts.
  - *MCP Unavailable*: If the `structural-editing` MCP server is not active or accessible in the agent's current environment, standard text-editing tools may be used.
  - *Unrecoverable MCP Failure*: If an AST operation encounters an unrecoverable parser defect or server failure, you may fall back to standard text tools after reporting the specific MCP condition.

---

## 6. Common Lisp Style & Conventions

- **Naming**:
  - Functions, variables, and macros use `kebab-case`.
  - Predicate functions end in `-p` (or `-p` for multi-word like `lisp-file-p`, `compound-node-p`).
  - Dynamic special variables are surrounded by asterisks: `*workspace-tree*`, `*file-registry*`.
  - Constants are surrounded by plus signs: `+protocol-version+`.
- **Packages**:
  - All symbols must be explicitly defined and exported in `defpackage` forms.
  - Maintain clean package separation: `conditions`, `utils`, `tree`, `parser`, `edit`, `analysis`, `refactor`, `workspace`, `mcp`.
- **Indentation & Formatting**:
  - Use standard 2-space Lisp indentation.
  - Never leave closing parentheses on lines by themselves.
  - Keep docstrings informative and clean.

---

## 7. Tool Selection & Agent Workflow Guide

> [!IMPORTANT]
> **Dogfooding Mandatory**: If the `structural-editing` MCP tools are available in your agent session, you **MUST** use them for all Lisp file inspections, edits, and refactorings in this project. Do not use text-based replacement or diff tools on Lisp files unless the MCP server is unavailable or the target is a non-Lisp file (Markdown, Nix, JSON, shell scripts).

The MCP server provides 19 tools organized into distinct operational tiers. Follow these patterns to edit code reliably without parenthetical corruption or parallel agent collisions:

### 1. The Standard Structural Editing Loop
Whenever you are asked to read, modify, or refactor code:
1. **Load & Inspect**:
   - Call `read_node` with `load_files: ["/path/to/project"]` on your first interaction to load target files into the workspace.
   - Inspect parent forms at depth 2 (e.g. `path: [0, 0, 5]`) to view the entire expression and its child paths simultaneously. Do NOT probe child indices one-by-one.
   - For large directories or files, use `limit` and `offset` in `read_node` to paginate cleanly without blowing up the context window.
   - Use `ast_search` to find symbols, functions, or text across the entire workspace quickly.
2. **Isolate / Branch (For Multi-Step or Multi-Agent Work)**:
   - For isolated experimentation or parallel work, call `workspace_manage` with `action: "fork"`, `source_id: "default"`, and `target_id: "agent-<id>"`.
   - Pass `workspace_id: "agent-<id>"` to all subsequent tool calls.
   - Create checkpoints before risky edits by forking a backup branch or using `workspace_manage` `action: "snapshot"`.
   - To create new files purely in memory without disk side-effects, call `workspace_create_file` (or `workspace_manage` with `action: "create_file"`).
3. **Apply Structural Transforms**:
   - `ast_modify`: Use `action: "overwrite"` to replace an entire sub-expression, `action: "insert"` to add forms into bodies or parameter lists, or `action: "wrap"` to enclose an expression in parens or a macro form (e.g. `(when condition)`).
   - `ast_remove`: Use `action: "delete"` to eliminate an unused node, `action: "unwrap"` to peel away an outer wrapper (e.g. removing `progn`), or `action: "promote"` to replace a parent with its child.
   - `ast_relocate`: Use `action: "move"` or `action: "copy"` to reorder top-level definitions or arguments, `action: "swap"` to interchange two expressions, or `action: "merge"` / `action: "split"` for collections.
   - *Instant Preview*: Every mutation tool automatically returns the rendered code snippet of the enclosing parent node. **Do not issue redundant `read_node` calls to verify mutations.**
4. **Semantic Refactoring & Search**:
   - `ast_rename`: Renames identifiers across the workspace or a subtree while safely ignoring string literals and comments.
   - `ast_replace_pattern`: Structural AST pattern replacement with wildcard variables (`?x`, `?y`). Use this for API migrations and macro upgrades instead of regex or diffs.
   - `ast_extract_variable`: Extracts sub-expressions into local `let` bindings.
   - `ast_extract_function`: Extracts sub-expressions into top-level functions and replaces call-sites.
5. **Quality Assurance & Linting**:
   - `ast_lint`: Identifies anti-patterns and code smells (e.g. `(if ... (progn ...))` $\rightarrow$ `when`).
   - `ast_complexity_metrics`: Computes cyclomatic complexity and nesting depth to locate functions needing decomposition.
   - `ast_find_duplicates`: Identifies repeated AST subtrees across files to extract shared utilities.
   - `ast_analyze_bindings`: Detects unused variables and lexical shadowing bugs.
   - `ast_suggest_refactorings`: Aggregates lint, complexity, duplicate, and binding analysis into a prioritized refactoring plan.
6. **Verify, Rebase, Merge, & Commit**:
   - Run `workspace_status` to see dirty vs clean files and current revision numbers.
   - Run `workspace_diff` between your branch and `"default"` to confirm disjoint modifications, auto-mergeable files, and detect collisions.
   - If upstream has diverged, call `workspace_rebase` to integrate upstream changes and resolve any collisions using 3-way AST merge or strategies (`"theirs"` / `"ours"`).
   - Run `workspace_merge` to fold your branch changes back into `"default"` (automatically performs AST-level disjoint merge across and within files).
   - Call `commit_workspace` to persist modified files to disk (supports optional `files` subset).

### 2. Multi-Agent Branching Matrix

| Tool | Primary Purpose | When to Use |
| :--- | :--- | :--- |
| `read_node` | AST & Workspace Inspection | First step to view code and obtain 0-indexed integer paths (supports `limit`/`offset` pagination). |
| `workspace_create_file` | In-memory file creation | Create a new file in memory without touching disk immediately. |
| `ast_modify` | Insert, overwrite, wrap nodes | Core AST surgery without risking unmatched parentheses. |
| `ast_remove` | Delete, unwrap, promote nodes | Eliminating dead code, stripping wrappers, promoting children. |
| `ast_relocate` | Move, copy, swap, merge, split | Moving functions, reordering parameters, combining lists. |
| `ast_replace_pattern` | AST pattern template replacement | Refactoring API patterns (e.g. `(old-fn ?a ?b)` $\rightarrow$ `(new-fn ?b :arg ?a)`). |
| `workspace_manage` | Lifecycle (`create_file`, `fork`, `rebase`, etc.) | Branching isolated agent workspaces and creating fallback points. |
| `workspace_rebase` | Rebase branch onto upstream | Bringing upstream changes into a feature branch and resolving AST collisions. |
| `workspace_status` | Status inspection | Checking dirty files and revision counters before merge/commit. |
| `workspace_diff` | AST difference comparison | Checking for collisions before merging two workspaces. |
| `workspace_merge` | Fast-forward, disjoint, or 3-way AST merge | Integrating an agent's changes into `"default"`. |
| `commit_workspace` | Disk persistence | Writing in-memory workspace AST back to source files on disk. |

---

## 8. Progressive Context: Skills & Rules

For specialized tasks, consult the `.agents/` directory:
- [`.agents/rules/architecture.md`](file:///.agents/rules/architecture.md): Deep-dive into AST node tags, pattern matching, and tree representations.
- [`.agents/rules/coding-standards.md`](file:///.agents/rules/coding-standards.md): Lisp idioms, macro best practices, and optimization guides.
- [`.agents/skills/test-and-debug/SKILL.md`](file:///.agents/skills/test-and-debug/SKILL.md): Running and debugging Rove test suites.
- [`.agents/skills/ast-transform-dev/SKILL.md`](file:///.agents/skills/ast-transform-dev/SKILL.md): Implementing new AST edits, relocations, or pattern transformations.
- [`.agents/skills/mcp-server-dev/SKILL.md`](file:///.agents/skills/mcp-server-dev/SKILL.md): Exposing new tools via the MCP protocol.
