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

## 3. Workspace Staging & File Registry

Defined in [`src/workspace.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/workspace.lisp):

- `*workspace-tree*`: Holds the global AST root.
- `*file-registry*`: Hash table mapping numerical file IDs and path tuples to physical filesystem paths.
- `load-into-workspace`: Loads a file or scans a directory recursively, classifying dialect by file extension:
  - Common Lisp: `.lisp`, `.cl`, `.asd`, `.lsp`
  - Clojure: `.clj`, `.cljs`, `.cljc`, `.edn`
  - Scheme: `.scm`, `.ss`, `.rkt`, `.sld`
  - Emacs Lisp: `.el`
  - Fennel: `.fnl`
- `write-workspace`: Writes modified AST files back to disk by unparsing the trees with dialect-appropriate pretty printing.

---

## 4. MCP Server & JSON-RPC Protocol

Defined in [`src/mcp.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/mcp.lisp):

- **Transport**: JSON-RPC 2.0 over standard I/O (stdio).
- **Initialization Handshake**: Responds to `initialize` and `notifications/initialized`.
- **Tools Discovery**: Responds to `tools/list` with JSON schema metadata for all 15 structural editing, refactoring, and analysis tools.
- **Tool Execution**: Dispatches `tools/call` requests to corresponding Lisp handlers.
- **Preview Mechanism**: Mutation tools generate an immediate structural code snippet preview of the modified node and its parent, allowing AI agents to visually verify AST modifications before committing.
