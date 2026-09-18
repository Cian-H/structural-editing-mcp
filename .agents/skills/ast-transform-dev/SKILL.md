---
name: ast-transform-dev
description: Guide for implementing and testing new AST mutations, relocations, and refactoring transforms.
---

# AST Transform Development Skill

This skill guides AI agents in extending or modifying AST transformation, relocation, pattern-replacement, and static analysis logic in `structural-editing-mcp`.

---

## 1. Relevant Source Files

- [`src/tree.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/tree.lisp): Core node definitions, path indexing, and pattern matching.
- [`src/parser.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/parser.lisp): Lexing, reading strings into ASTs, and pretty-printing trees back to strings.
- [`src/edit.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/edit.lisp): Core structural mutations (`ast-modify`, `ast-remove`, `ast-relocate`).
- [`src/refactor.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/refactor.lisp): Semantic transforms (pattern replacement, let extraction, function extraction).
- [`src/analysis.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/analysis.lisp): Linters, complexity metrics, clone detection, and binding analysis.

---

## 2. Implementing a Structural Mutation

All mutations follow a functional update pattern:

1. **Locate Target Node**: Use `structural-editing-mcp.tree:get-node-at-path` to find the target node in `*workspace-tree*`.
2. **Validate Input**: Check that the operation makes sense (e.g. cannot unwrap an atom, cannot split an empty list). If invalid, signal an error from `conditions.lisp`.
3. **Construct New Subtree**: Construct the replacement node with updated children.
4. **Re-index Paths**: Call `(structural-editing-mcp.tree:reindex-paths new-node base-path)` to ensure every descendant node contains accurate coordinate paths.
5. **Update Workspace**: Call `(structural-editing-mcp.tree:update-node-at-path *workspace-tree* path new-node)` to produce the new workspace tree.
6. **Generate Preview**: Return the updated enclosing parent form as a preview string using `structural-editing-mcp.parser:unparse-node`.

---

## 3. Handling Multi-Dialect Rules

When transforming nodes, consider the dialect of the enclosing file:

- **Common Lisp**: Delimiters are `:paren` `(...)`.
- **Clojure**:
  - Delimiters may be `:paren` `(...)`, `:square` `[...]`, or `:curly` `{...}`.
  - Parameter vectors and `let` binding forms use `:square`.
- **Scheme**:
  - Binding pairs in `let`, `let*`, `letrec` commonly use `:square` `[var val]`.
- **Preserve Comments**:
  - Comments are stored as `(:path ... :comment text)`.
  - When filtering or transforming child nodes, ensure comment nodes are not inadvertently discarded.

---

## 4. Testing Your AST Transform

Always create corresponding unit tests in `test/edit.lisp` or `test/analysis.lisp`:

```lisp
(deftest test-my-transform
  (let* ((source "(defun foo (x) (+ x 1))")
         (ast (structural-editing-mcp.parser:parse-string source :common-lisp))
         (transformed (my-transform ast '(3))))
    (ok (search "expected-code" (structural-editing-mcp.parser:unparse-node transformed :common-lisp)))))
```

Run your new test suite:
```bash
./scripts/run-tests.lisp test-edit
```
