# Common Lisp Coding Standards & Guidelines

> Coding conventions and guidelines for AI agents modifying `structural-editing-mcp`.

---

## 1. Safety & Compilation Declaims

All source files should declare defensive optimization settings at the top:
```lisp
(declaim (optimize (speed 2) (safety 3)))
```
- `safety 3` ensures SBCL performs full type and bounds checking, avoiding hard segfaults.
- Inline hot accessors (`get-node-path`, `get-node-tag`, `get-node-children`) with:
  ```lisp
  (declaim (inline get-node-path get-node-tag get-node-children))
  ```

---

## 2. Naming Conventions

- **Identifiers**: Use lowercase `kebab-case` for all functions, macros, variables, and package names.
- **Predicates**: Suffix with `-p` (e.g. `lisp-file-p`, `compound-node-p`, `valid-path-p`).
- **Special / Global Variables**: Wrap in asterisks (`*workspace-tree*`, `*file-registry*`).
- **Constants**: Wrap in plus signs (`+protocol-version+`).
- **Keyword Arguments**: Prefix with a descriptive keyword, e.g., `&key (depth 2) (exact t)`.

---

## 3. Pattern Matching with Trivia

Prefer `trivia:match` over nested `car`/`cdr`/`cadr` or chained `typecase`:

```lisp
(trivia:match node
  ((structural-editing-mcp.tree:leaf path val)
   (handle-leaf path val))
  ((structural-editing-mcp.tree:comment path text)
   (handle-comment path text))
  ((structural-editing-mcp.tree:node path tag children)
   (handle-children path tag children))
  (_
   (error 'invalid-node-error :node node)))
```

This guarantees pattern exhaustiveness, readability, and structural safety.

---

## 4. Condition Handling & Error Hierarchy

Defined in [`src/conditions.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/conditions.lisp):

- Subtype from `mcp-error` or standard conditions:
  ```lisp
  (define-condition invalid-path-error (mcp-error)
    ((path :initarg :path :reader error-path))
    (:report (lambda (condition stream)
               (format stream "Invalid or non-existent AST path: ~A" (error-path condition)))))
  ```
- **Never allow unhandled conditions to leak to the SBCL interactive debugger** during headless agent sessions or MCP server execution.
- Catch conditions in tool dispatchers using `handler-case`:
  ```lisp
  (handler-case
      (execute-tool-action params)
    (invalid-path-error (c)
      (send-error id -32602 (format nil "~A" c)))
    (error (c)
      (send-error id -32603 (format nil "Internal error: ~A" c))))
  ```

---

## 5. Stdio Protocol Safety

- **Strict Stdio Isolation**: Standard output (`*standard-output*`) is the JSON-RPC wire protocol transport.
- Never write debug logs, print calls, or ASDF messages to stdout.
- Direct all diagnostics to `*error-output*`:
  ```lisp
  ;; Good
  (format *error-output* "~&[DEBUG] Loaded file: ~A~%" filepath)

  ;; FORBIDDEN: Will corrupt MCP JSON-RPC protocol
  (format t "~&[DEBUG] Loaded file: ~A~%" filepath)
  (print filepath)
  ```

---

## 6. Functional Tree Updates

AST nodes are represented as S-expression property lists. Prefer functional updates that return a new node rather than mutating in place:
```lisp
;; Good: functional update returning new tree with reindexed paths
(let ((new-children (replace-child-at children target-idx replacement)))
  (reindex-paths (list* :path path tag new-children) path))
```
Update the workspace reference atomically at the top level:
```lisp
(setf *workspace-tree* new-workspace-tree)
```

---

## 7. Package Cleanliness & Dependencies

- Never pollute the `COMMON-LISP-USER` (`CL-USER`) package.
- Always use explicit `in-package` after `defpackage`.
- Leverage `:use` or package nicknames cleanly.
- Export all public APIs in `defpackage` so other modules and the test suite can access them cleanly.

---

## 8. Structural Editing & Dogfooding Directive

- **Dogfood Our Own MCP**: When the `structural-editing` MCP tools are available in the agent's tool environment, always use them to read, inspect, modify, and refactor Lisp files (`.lisp`, `.cl`, `.asd`).
- **No Text Diffs on Lisp Code**: Do not perform direct string replacements or text diffs on Lisp source files when the MCP server is available.
- **Exceptions**: Non-Lisp files (Markdown, JSON, Nix, Shell) and emergency fallbacks when the MCP server is offline.

