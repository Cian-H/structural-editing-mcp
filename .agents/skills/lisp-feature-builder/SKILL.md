---
name: lisp-feature-builder
description: >-
  Workflow for safely adding new features to the Lisp codebase. Integrates Jev System One semantic navigation, isolated workspace branching, and pre-merge safety reviews.
---
# Lisp Feature Builder

## Overview
This skill outlines the standard operating procedure for adding new features or logic to the `structural-editing-mcp` codebase. It combines isolated AST workspace branching, System 1 semantic navigation, and rigorous safety gating to ensure new features don't destabilize the core parser or editing functions.

## Dependencies
- **lisp-jev-ast-navigator**: Used to find insertion points.
- **lisp-jev-refactor-evaluator**: Used to lint new code.
- **structural-editing-mcp**: For branching and AST mutations.
- **jev**: For safety gating (`jev_review`).

## Workflow

### 1. Locate Insertion Points (System 1)
- Use the `lisp-jev-ast-navigator` skill to semantically search the codebase and identify the exact AST coordinates where the new feature should hook in (e.g., finding the relevant `defpackage` to export symbols, or the target `defun` to extend).

### 2. Branch the Workspace
- Call `workspace_manage` with `action: "fork"` to create an isolated feature branch (e.g., `workspace_id: "feat-xyz"`). Do not develop new features directly on `"default"`.

### 3. Implement the Feature (AST Surgery)
- Use the mutation tools (`ast_modify` with `action: "insert"`, `ast_relocate`, etc.) to build the feature in memory.
- If creating entirely new files, use `workspace_create_file`.

### 4. Self-Audit (System 1)
- Run `ast_suggest_refactorings` on the nodes you just created.
- Filter the results through the `lisp-jev-refactor-evaluator` skill to ensure your new feature is idiomatic and clean.

### 5. Verify & Test
- Execute `./scripts/run-tests.lisp`. You must achieve 0 failures.

### 6. Merge Gating (`jev_review`)
- Before calling `workspace_merge` to push your feature to the `"default"` branch, pass your AST diff to the `jev_review` tool.
- If Jev flags a high `blast_radius` or low `safe_to_apply` score, rethink your architecture (e.g., you might be modifying a core macro unnecessarily).
- Once Jev approves, merge the workspace and optionally call `commit_workspace` to save to disk.
