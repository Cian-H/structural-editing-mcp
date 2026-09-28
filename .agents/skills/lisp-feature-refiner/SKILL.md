---
name: lisp-feature-refiner
description: >-
  Workflow for iterating on and refining existing features in the Lisp codebase using structural-editing MCP tools. Uses System 1 to choose the best implementation approach, verify the refinement intent, and gate merges.
---

# Lisp Feature Refiner

## Overview
This skill provides a structured loop for improving, extending, or optimizing an existing feature. Instead of making risky, ad-hoc edits, the agent identifies the target AST node, synthesizes competing refinement strategies, uses `jev_decide` (System 1) to pick the most idiomatic path, applies the changes strictly via `structural-editing` MCP tools, and verifies the diff with `jev_verify` and `jev_review` before merging.

## Strict Dogfooding Invariants
- **NEVER use text-based replacement tools (`replace_file_content`, text diffs) on Lisp files (`.lisp`, `.cl`, `.asd`).**
- **ALL modifications MUST be performed in-memory via `structural-editing` MCP tools.**
- **Changes persist to disk ONLY via `commit_workspace`.**

## Toolchain & MCP Mappings
- **Structural Editing (`ServerName: "structural-editing"`):**
  - `read_node` / `read_slice`: Inspect node and parent context.
  - `workspace_manage`: Branching (`action: "fork"`).
  - `ast_modify`: Rewrite expressions (`action: "overwrite"`), wrap (`action: "wrap"`), insert (`action: "insert"`).
  - `ast_extract_function`: Break complex forms into dedicated helper functions.
  - `ast_replace_pattern`: Structural AST pattern replacement with wildcards (`?x`, `?y`).
  - `ast_remove`: Remove dead code (`action: "delete"`), unwrap (`action: "unwrap"`), promote children (`action: "promote"`).
  - `workspace_diff`: Review AST diff against `"default"`.
  - `workspace_merge`: Merge branch into `"default"`.
  - `commit_workspace`: Persist dirty files to disk.
- **System 1 Engine (`ServerName: "jev"`):**
  - `jev_find`: Fast semantic navigation to target function.
  - `jev_decide`: Select the best implementation approach among competing candidates.
  - `jev_verify`: Verify the AST diff fulfills the user's refinement intent.
  - `jev_review`: Assess safety and blast radius.

---

## Step-by-Step Execution Workflow

### Step 1: Locate the Target AST Node (System 1)
1. Ensure the workspace is loaded via `read_node(load_files: ["..."])`.
2. Use `lisp-jev-ast-navigator` (`jev_find` over the skeleton) to pinpoint the exact integer path of the function/macro to refine (e.g. `[0, 1, 42]`).
3. Call `read_node` on that exact path to inspect the full form:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "read_node",
     "Arguments": { "path": [0, 1, 42], "depth": 2 }
   }
   ```

### Step 2: Approach Selection (`jev_decide`)
Before modifying code, formulate 2-3 distinct implementation candidates based on the user's refinement request:
- Candidate A: Extract a helper function via `ast_extract_function`.
- Candidate B: Replace structural pattern via `ast_replace_pattern`.
- Candidate C: In-place expression rewrite via `ast_modify(action: "overwrite")`.

Query Jev to select the most idiomatic candidate:
```json
{
  "ServerName": "jev",
  "ToolName": "jev_decide",
  "Arguments": {
    "decision": "Which implementation approach is most idiomatic, performant, and safe?",
    "evidence": "<Current AST rendered code and user refinement goals>",
    "priorities": "Prioritize idiomatic Common Lisp style, maintainability, and minimal blast radius.",
    "candidates": [
      { "id": "approach_a", "description": "Extract helper function for recursive clause" },
      { "id": "approach_b", "description": "Use ast_replace_pattern to modernize macro calls" }
    ]
  }
}
```

### Step 3: Fork Workspace (`workspace_manage`)
Create an isolated branch:
```json
{
  "ServerName": "structural-editing",
  "ToolName": "workspace_manage",
  "Arguments": {
    "action": "fork",
    "source_id": "default",
    "target_id": "refine-<name>"
  }
}
```

### Step 4: Execute Refinement via Structural Editing Tools
Execute the selected approach in the isolated branch (pass `"workspace_id": "refine-<name>"`):

- **If extracting functions:**
  ```json
  {
    "ServerName": "structural-editing",
    "ToolName": "ast_extract_function",
    "Arguments": {
      "path": [0, 1, 42, 5, 2],
      "function_name": "clean-helper-fn",
      "workspace_id": "refine-<name>"
    }
  }
  ```
- **If replacing patterns:**
  ```json
  {
    "ServerName": "structural-editing",
    "ToolName": "ast_replace_pattern",
    "Arguments": {
      "pattern": "(old-api ?x ?y)",
      "replacement": "(new-api ?y :setting ?x)",
      "workspace_id": "refine-<name>"
    }
  }
  ```
- **If overwriting sub-expressions:**
  ```json
  {
    "ServerName": "structural-editing",
    "ToolName": "ast_modify",
    "Arguments": {
      "action": "overwrite",
      "path": [0, 1, 42, 5],
      "new_node": "(when (valid-p x) (process x))",
      "workspace_id": "refine-<name>"
    }
  }
  ```

### Step 5: Verify Intent & Safety (System 1)
1. Obtain AST diff:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_diff",
     "Arguments": {
       "source_workspace_id": "refine-<name>",
       "target_workspace_id": "default"
     }
   }
   ```
2. Verify intent with `jev_verify`:
   Pass user's refinement goal as `claim` and the AST diff as `evidence`. Must return `verified`.
3. Check safety with `jev_review`:
   Verify `safe_to_apply` is high and `blast_radius` is low.

### Step 6: Test & Merge
1. Run `./scripts/run-tests.lisp`. Must exit with 0 failures.
2. Merge into `"default"`:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_merge",
     "Arguments": {
       "source_workspace_id": "refine-<name>",
       "target_workspace_id": "default"
     }
   }
   ```
3. Commit to disk:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "commit_workspace",
     "Arguments": { "workspace_id": "default" }
   }
   ```
