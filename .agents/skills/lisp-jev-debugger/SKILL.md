---
name: lisp-jev-debugger
description: >-
  Workflow for diagnosing and fixing bugs in the Lisp codebase. Uses Jev System One to verify bug-fix claims and isolate blast radius.
---
# Lisp Jev Debugger

## Overview
This skill provides a structured methodology for tracking down and fixing bugs in the `structural-editing-mcp` project. It ensures that bug fixes are highly targeted, verified against the original bug report using System 1 intent checking, and thoroughly tested before merging.

## Dependencies
- **lisp-jev-ast-navigator**: Used to find faulty logic.
- **structural-editing-mcp**: For isolated AST mutations.
- **jev**: For fix verification (`jev_verify`) and safety review (`jev_review`).

## Workflow

### 1. Reproduce & Locate (System 1)
- Analyze the user's bug description or stack trace.
- Use the `lisp-jev-ast-navigator` skill to semantically locate the specific AST node responsible for the faulty logic.

### 2. Isolate the Fix
- Call `workspace_manage` with `action: "fork"` to create a debug workspace (e.g., `workspace_id: "bugfix-123"`).

### 3. Apply the Fix
- Use `ast_modify` (with `action: "overwrite"`) or `ast_replace_pattern` to patch the bug at the exact AST coordinate. Avoid wide parent-level overwrites to prevent collateral damage.

### 4. Intent Verification (`jev_verify`)
- Before testing, pass the user's original bug description as the `claim` and your proposed AST diff as the `evidence` to the `jev_verify` tool.
- Jev will instantly check if your diff actually addresses the bug. If it returns `contradicted` or `unsupported`, your fix missed the mark—try again.

### 5. Test Suite Execution
- Run `./scripts/run-tests.lisp`. 
- Ensure that not only is the bug fixed, but no regressions were introduced.

### 6. Safety Check & Merge
- Pass the final AST diff to `jev_review`. Bug fixes should typically have a very low `blast_radius`. If Jev flags a high blast radius, your fix is likely too broad and might cause unintended side effects.
- Call `workspace_merge` to integrate the fix into `"default"`, then `commit_workspace` to persist it.
