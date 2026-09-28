---
name: lisp-feature-refiner
description: >-
  Workflow for iterating on and refining existing features in the Lisp codebase. Uses System 1 to choose the best implementation approach, verify the refinement intent, and gate merges.
---
# Lisp Feature Refiner

## Overview
This skill provides a structured loop for improving, extending, or optimizing an existing feature. Instead of blindly overwriting code with the first idea that comes to mind, the agent generates multiple refinement approaches, uses Jev to select the most idiomatic path, and verifies the final result against the user's intent.

## Dependencies
- **lisp-jev-ast-navigator**: Used to locate the feature being refined.
- **structural-editing-mcp**: For isolated AST mutations.
- **jev**: For approach selection (`jev_decide`), intent verification (`jev_verify`), and safety gating (`jev_review`).

## Workflow

### 1. Locate the Feature (System 1)
- Take the user's description of the feature to refine.
- Use `lisp-jev-ast-navigator` to semantically jump to the exact AST coordinates of the target functions or macros.

### 2. Formulate Refinement Candidates
- Analyze the current AST and generate 2-3 distinct implementation candidates for the requested refinement (e.g., "Extract a recursive helper", "Use a built-in mapping function", "Rewrite using a macro").
- Call `jev_decide`. Provide the user's request as the `decision`, the current AST as the `evidence`, and your proposed implementation strategies as the `candidates`.
- Ask Jev to select the candidate that is the most idiomatic, performant, and safe.

### 3. Isolate the Refinement
- Call `workspace_manage` with `action: "fork"` to create a refinement workspace (e.g., `workspace_id: "refine-abc"`).

### 4. Execute the Refinement
- Use structural editing tools (`ast_modify`, `ast_replace_pattern`, `ast_extract_function`) to apply the chosen approach.

### 5. Intent Verification (`jev_verify`)
- Pass the user's specific refinement request as the `claim` and the resulting AST diff as the `evidence` to `jev_verify`.
- Ensure Jev returns `verified` before proceeding. If it returns `contradicted` or `unsupported`, revisit your execution.

### 6. Test & Merge Gating (`jev_review`)
- Run `./scripts/run-tests.lisp` to ensure existing functionality remains intact.
- Pass the final AST diff to `jev_review`. Refinements shouldn't inadvertently bloat the `blast_radius`.
- Call `workspace_merge` to push the refined feature back into `"default"`, then `commit_workspace`.
