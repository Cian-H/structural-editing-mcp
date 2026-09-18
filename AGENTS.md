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
3. **In-Memory Staging**: Modifications stage safely in memory (`*workspace-tree*`) with instant previews and are persisted to disk only when explicitly committed via `commit_workspace`.
4. **Static Analysis & Refactoring**: Linting, cyclomatic complexity metrics, code duplicate detection, lexical binding/shadowing analysis, and pattern replacement.

---

## 2. Environment & Prerequisites

The project uses [devenv](https://devenv.sh/) with Nix for hermetic, reproducible dependencies.

- **Primary Runtime**: SBCL (Steel Bank Common Lisp)
- **Key Common Lisp Libraries**:
  - `trivia`: Pattern matching on AST nodes
  - `alexandria` & `serapeum`: Standard utility libraries
  - `yason`: JSON parsing and serialization for MCP JSON-RPC
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
| **Compile Standalone Binary** | `./scripts/build.lisp` | Builds `structural-editing-mcp-server` via `save-lisp-and-die`. |
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

## 7. Progressive Context: Skills & Rules

For specialized tasks, consult the `.agents/` directory:
- [`.agents/rules/architecture.md`](file:///.agents/rules/architecture.md): Deep-dive into AST node tags, pattern matching, and tree representations.
- [`.agents/rules/coding-standards.md`](file:///.agents/rules/coding-standards.md): Lisp idioms, macro best practices, and optimization guides.
- [`.agents/skills/test-and-debug/SKILL.md`](file:///.agents/skills/test-and-debug/SKILL.md): Running and debugging Rove test suites.
- [`.agents/skills/ast-transform-dev/SKILL.md`](file:///.agents/skills/ast-transform-dev/SKILL.md): Implementing new AST edits, relocations, or pattern transformations.
- [`.agents/skills/mcp-server-dev/SKILL.md`](file:///.agents/skills/mcp-server-dev/SKILL.md): Exposing new tools via the MCP protocol.
