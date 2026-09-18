# Structural Editing MCP

A Model Context Protocol (MCP) server providing AST-level structural editing,
semantic refactoring, and static analysis for Lisp family languages.

`structural-editing-mcp` allows AI coding assistants (such as Claude, Cursor,
and Antigravity) to inspect, manipulate, and analyze Lisp codebases directly as
Abstract Syntax Trees rather than brittle text strings or line-based diffs. This
eliminates syntax errors, mismatched parentheses, and indentation breakage
during automated code modifications.

--------------------------------------------------------------------------------

## Key Features

- **AST-Aware Structural Surgery**: Insert, overwrite, wrap, delete, unwrap,
  promote, move, copy, swap, split, and merge expressions safely at the syntax
  tree level.
- **Multi-Dialect Support**: First-class support for Common Lisp, Clojure,
  Scheme / Racket, Emacs Lisp, and Fennel.
- **Precise Path Addressing**: Hierarchical integer-path addressing
  (`[dialect, file, form, ...]`) allows direct, unambiguous access to any
  expression or atom.
- **In-Memory Staging & Safe Disk Persistence**: Stage complex, multi-file
  refactoring sessions in an in-memory workspace tree. Receive immediate
  contextual previews after every mutation; commit to disk only when ready.
- **High-Level Refactoring Primitives**: Fast workspace-wide symbol search,
  semantic bulk renaming, structural pattern replacement with wildcards,
  `let`-binding extraction, and function extraction.
- **Comprehensive Static Analysis**: Structural anti-pattern linting, cyclomatic
  complexity & nesting depth metrics, code clone/duplicate detection, lexical
  variable binding analysis (detecting unused and shadowed variables), and a
  unified refactoring advisor.

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

- `[0, 0, 0]` addresses the entire `defun` form:
  `(defun add-numbers (a b) (+ a b))`
- `[0, 0, 0, 0]` addresses the symbol `defun`
- `[0, 0, 0, 1]` addresses the function name `add-numbers`
- `[0, 0, 0, 2]` addresses the parameter list `(a b)`
- `[0, 0, 0, 2, 0]` addresses parameter `a`
- `[0, 0, 0, 3]` addresses the body expression `(+ a b)`
- `[0, 0, 0, 3, 1]` addresses variable reference `a` inside the body

--------------------------------------------------------------------------------

## Tool Reference

The MCP server exposes 15 specialized tools across four functional categories:

### 1. Workspace & Navigation

  | Tool               | Parameters                                                                 | Description                                                                                                                                                                                             |
  | :----------------- | :------------------------------------------------------------------------- | :------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
  | `read_node`        | `path` *(optional)*<br>`depth` *(default: 2)*<br>`load_files` *(optional)* | Inspects an AST node or the entire workspace. Returns formatted source code and a hierarchy of child paths up to `depth`. Pass directory or file paths in `load_files` to load them into the workspace. |
  | `commit_workspace` | *(none)*                                                                   | Writes all staged in-memory workspace modifications back to their respective source files on disk.                                                                                                      |

### 2. Structural Editing (AST Surgery)

All mutation tools automatically return an updated structural preview of the
enclosing parent node.

  | Tool           | Parameters                                                                                                                                            | Description                                                                                                                                                                                                                                                                                      |
  | :------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
  | `ast_modify`   | `path`<br>`action`: `"insert"` \| `"overwrite"` \| `"wrap"`<br>`new_node`<br>`index` *(optional)*<br>`end_index` *(optional)*                         | **`insert`**: Adds `new_node` before `path` (or at child `index`).<br>**`overwrite`**: Replaces the node at `path` with `new_node`.<br>**`wrap`**: Wraps the node (or child range `index` to `end_index`) in `:paren`, `:square`, `:curly`, or an enclosing form string (e.g. `(when valid-p)`). |
  | `ast_remove`   | `path`<br>`action`: `"delete"` \| `"unwrap"` \| `"promote"`                                                                                           | **`delete`**: Removes the node at `path`.<br>**`unwrap`**: Strips the outer collection, spilling its children into the parent.<br>**`promote`**: Replaces the parent node with the child at `path`.                                                                                              |
  | `ast_relocate` | `source_path` *(optional for split)*<br>`target_path`<br>`action`: `"move"` \| `"copy"` \| `"swap"` \| `"merge"` \| `"split"`<br>`index` *(optional)* | **`move`** / **`copy`**: Moves or duplicates `source_path` to `target_path` at `index`.<br>**`swap`**: Exchanges the nodes at `source_path` and `target_path`.<br>**`merge`**: Joins two adjacent collection nodes into one.<br>**`split`**: Divides a collection into two at child `index`.     |

### 3. Refactoring & Transformations

  | Tool                   | Parameters                                         | Description                                                                                                                                                           |
  | :--------------------- | :------------------------------------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
  | `ast_search`           | `query`<br>`path` *(optional)*                     | Fast search for symbol names, function identifiers, or tokens across the workspace or within a specific subtree. Returns exact AST paths to each match.               |
  | `ast_rename`           | `old_name`<br>`new_name`<br>`path` *(optional)*    | Bulk renames a leaf symbol across the entire workspace or scoped under a specific AST subtree.                                                                        |
  | `ast_replace_pattern`  | `pattern`<br>`replacement`                         | Searches for structural patterns and replaces them with a template, preserving captured wildcard variables (e.g. pattern `(foo ?x ?y)` to replacement `(bar ?y ?x)`). |
  | `ast_extract_variable` | `path`<br>`variable_name`                          | Extracts the sub-expression at `path` into a local `let` binding wrapped around its parent form.                                                                      |
  | `ast_extract_function` | `path`<br>`function_name`<br>`params` *(optional)* | Extracts the sub-expression at `path` into a new top-level `defun`, replacing the original expression with a call to the new function.                                |

### 4. Static Analysis & Code Quality

  | Tool                       | Parameters                                                                                                                  | Description                                                                                                                                                                                                                                                 |
  | :------------------------- | :-------------------------------------------------------------------------------------------------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
  | `ast_lint`                 | `path` *(optional)*<br>`dialect` *(optional)*<br>`rules` *(optional)*                                                       | Structural linter detecting anti-patterns and code smells (e.g. `if-progn-to-when`, `if-nil-to-when`, `if-not-to-unless`, `single-clause-cond`, `redundant-progn`, `nested-let`, `equal-nil-to-null`). Returns findings with AST paths and suggested fixes. |
  | `ast_complexity_metrics`   | `path` *(optional)*<br>`min_complexity` *(default: 1)*<br>`min_depth` *(default: 1)*<br>`dialect` *(optional)*              | Computes cyclomatic complexity, maximum nesting depth, and AST node counts for functions and forms. Provides automated refactoring recommendations for complex forms.                                                                                       |
  | `ast_find_duplicates`      | `path` *(optional)*<br>`min_nodes` *(default: 4)*<br>`min_depth` *(default: 2)*<br>`exact` *(default: true)*                | Detects repeated expressions and code clones across the workspace. Identifies opportunities to extract common logic into helper functions or variables.                                                                                                     |
  | `ast_analyze_bindings`     | `path` *(optional)*<br>`include_unused` *(default: true)*<br>`include_shadowed` *(default: true)*<br>`dialect` *(optional)* | Lexical scope analyzer that flags unused variables and shadowed bindings across function arguments, `let`, `labels`, `flet`, `lambda`, and loop macros.                                                                                                     |
  | `ast_suggest_refactorings` | `path` *(optional)*<br>`min_priority` *(default: "low")*<br>`categories` *(optional)*<br>`dialect` *(optional)*             | Multi-engine refactoring advisor that aggregates and prioritizes findings across linting, complexity, duplicates, and binding analysis into an actionable plan.                                                                                             |

--------------------------------------------------------------------------------

## Versioning

The project uses Calendar Versioning (CalVer).

- `version.txt` holds the current version in `YYYY.M.D.N` format (e.g.,
  `2026.9.18.53`).
- The version is read by ASDF and exposed via `+version+` in `src/version.lisp`.
- The command‑line flag `--version` (or `-v`) prints the version.
- The helper script `scripts/calver.lisp` provides utilities:
  - `--print` -- prints the current version.
  - `--next` -- shows the next CalVer for today.
  - `--update` -- updates `version.txt` to the latest CalVer.
  - `--tag` -- creates a matching Git tag `v<version>` if absent.
  - `--check` -- validates that `version.txt` matches the computed CalVer.
- A **pre‑push Git hook** (`.githooks/pre‑push`) runs `calver.lisp --update`,
  commits any change to `version.txt`, and creates the tag before the push. The
  hook is activated automatically in the development shell via `devenv.nix`.
- On every push to `main`, GitHub Actions (`.github/workflows/release.yml`)
  reads `version.txt`, skips if that CalVer already has a GitHub Release, and
  otherwise runs tests, builds a portable Linux x86_64 binary, and publishes it
  to the Releases page.

For CI pipelines you can invoke `./scripts/calver.lisp --check` to enforce
version consistency.

## Getting Started

### Prerequisites

- [SBCL](https://www.sbcl.org/) (Steel Bank Common Lisp)
- [devenv](https://devenv.sh/) (optional, recommended for reproducible Nix
  environments) or Quicklisp
- Common Lisp dependencies: `trivia`, `alexandria`, `serapeum`, `yason`, `rove`

### Installation & Client Configuration

You can run `structural-editing-mcp` using the built standalone binary or
directly through SBCL/devenv via the JSON-RPC stdio transport.

#### Claude Desktop

Add to your `claude_desktop_config.json`:

```json
{
  "mcpServers": {
    "structural-editing": {
      "command": "/path/to/structural-editing-mcp/semcp"
    }
  }
}
```

Or using `devenv`:

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

#### Cursor / Antigravity

Add to your MCP server configuration (`.cursor/mcp.json` or Antigravity
configuration):

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

## Development

### Running the Server

Start the server directly over standard I/O:

```bash
./scripts/run-server.lisp
```

### Building the Standalone Executable

Build a self-contained compressed binary (`semcp`):

```bash
./scripts/build.lisp
```

### Running the Test Suite

Run all unit and integration tests using
[Rove](https://github.com/fukamachi/rove):

```bash
./scripts/run-tests.lisp
```

To run a specific test suite:

```bash
./scripts/run-tests.lisp parser-tests edit-tests
```

--------------------------------------------------------------------------------

## Project Architecture

```text
structural-editing-mcp/
├── scripts/
│   ├── build.lisp          # Standalone binary compiler (sb-ext:save-lisp-and-die)
│   ├── run-server.lisp     # Stdio MCP server runner
│   └── run-tests.lisp      # Test suite runner
├── src/
│   ├── tree.lisp           # AST representations, pattern matching, path indexing
│   ├── parser.lisp         # Multi-dialect lexer, reader, and pretty-printer
│   ├── edit.lisp           # Functional AST surgery primitives (insert, wrap, move, etc.)
│   ├── analysis.lisp       # Linter, complexity calculator, clone finder, binding tracker
│   ├── refactor.lisp       # Pattern replacement, let-binding and function extraction
│   ├── workspace.lisp      # Multi-file workspace registry, dialect routing, disk I/O
│   ├── mcp.lisp            # MCP JSON-RPC protocol server and tool handlers
│   ├── conditions.lisp     # Condition types and error definitions
│   ├── utils.lisp          # General helper functions
│   └── main.lisp           # System entry point
├── test/                   # Comprehensive Rove test suite
├── devenv.nix              # Reproducible Nix environment specification
├── structural-editing-mcp.asd # ASDF system definition
└── license.md              # GNU Lesser General Public License v3.0
```

--------------------------------------------------------------------------------

## License

Distributed under the terms of the GNU Lesser General Public License v3.0
(LGPLv3). See [license.md](license.md) for details.
