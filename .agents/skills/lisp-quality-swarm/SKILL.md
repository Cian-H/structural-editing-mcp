---
name: lisp-quality-swarm
description: >-
  Deploys an automated swarm of worker subagents via invoke_subagent to perform code quality passes on the Lisp codebase. Uses Jev System One models to filter findings, verify worker intent, and review AST diffs for safety before merging via structural-editing MCP tools.
---

# Lisp Code Quality Swarm

## Overview
This skill orchestrates a real agent swarm to audit, evaluate, and optimize a Lisp codebase. The calling agent acts as the **Orchestrator**, running static analysis, filtering findings with the Jev System One decision engine, and **actually deploying parallel worker subagents via `invoke_subagent`**. Each worker operates in an isolated AST branch using the `structural-editing` MCP tools, and changes are gated with `jev_review` and `jev_verify` before being merged and committed to disk.

## Strict Dogfooding Invariants
- **NEVER use text-based replacement or diff tools (`replace_file_content`, text diffs) on Lisp files (`.lisp`, `.cl`, `.asd`).**
- **ALL modifications MUST be performed in-memory via `structural-editing` MCP tools.**
- **Changes persist to disk ONLY via `commit_workspace`.**

## Toolchain & MCP Mappings
- **Structural Editing (`ServerName: "structural-editing"`):**
  - `read_node`: Load files into workspace (`load_files`) and inspect AST coordinates.
  - `ast_suggest_refactorings`: Aggregate lint, complexity, duplicate, and binding analysis.
  - `workspace_manage`: Manage lifecycle (`action: "fork"` to branch, `action: "list"` to inspect).
  - `workspace_diff`: Compute AST diff between worker branch and `"default"`.
  - `workspace_merge`: Merge worker branch into `"default"`.
  - `commit_workspace`: Persist dirty files to disk.
- **System 1 Engine (`ServerName: "jev"`):**
  - `jev_decide`: Filter findings via `lisp-jev-refactor-evaluator`.
  - `jev_review`: Review AST diffs for `safe_to_apply` and `blast_radius`.
  - `jev_verify`: Verify worker changes match the assigned refactoring intent.
- **Subagent Swarm Management:**
  - `invoke_subagent`: Launch worker subagents concurrently.

---

## Orchestrator Execution Workflow

### Step 1: Workspace Ingestion & Automated Audit
1. Call `read_node` (with `load_files: ["/path/to/project"]` or target file) to ensure all source files are loaded into `*workspace-tree*`.
2. Call `ast_suggest_refactorings` on target path (or workspace root `path: []`) with `min_priority: "style"`:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "ast_suggest_refactorings",
     "Arguments": { "path": [0, 0], "min_priority": "style" }
   }
   ```

### Step 2: System 1 Finding Filtration (`jev_decide`)
For each finding returned by `ast_suggest_refactorings`:
1. Use the `lisp-jev-refactor-evaluator` workflow to query `jev_decide`.
2. Inspect Jev's `confidence` and `selected` candidate:
   - If `confidence < 0.68` OR `selected != "apply"`: **Silently discard**.
   - If `confidence >= 0.68` AND `selected == "apply"`: **Approve for worker delegation**.

### Step 3: Termination Check (System 2)
- If 0 approved findings remain: Terminate the swarm, present a summary of work completed, and stop.
- If approved findings exist, proceed to Step 4.

### Step 4: Branch Isolation (`workspace_manage`)
For each approved finding `i` with target coordinate `path` (e.g. `[0, 0, 104]`):
1. Call `workspace_manage` to create an isolated branch:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_manage",
     "Arguments": {
       "action": "fork",
       "source_id": "default",
       "target_id": "worker-branch-<i>"
     }
   }
   ```

### Step 5: Launch Swarm via `invoke_subagent`
Deploy worker subagents concurrently using the `invoke_subagent` tool. Group approved delegations into a single call:

```json
{
  "Subagents": [
    {
      "TypeName": "self",
      "Role": "AST Extraction Specialist",
      "Model": "flash",
      "Workspace": "inherit",
      "Prompt": "You are a specialized worker in a structural editing swarm. You must optimize the AST node at path [0, 0, 104] in workspace 'worker-branch-1'.\n\nMANDATORY RULES:\n1. Target workspace is 'worker-branch-1'. Every structural-editing call MUST pass \"workspace_id\": \"worker-branch-1\".\n2. DO NOT use text-editing tools (replace_file_content). Use ONLY structural-editing MCP tools: ast_extract_function, ast_modify, ast_remove, ast_replace_pattern.\n3. Verify newly extracted helper names follow Common Lisp conventions (kebab-case, -p for predicates).\n4. Report back when the AST modification is complete with the exact tool call used."
    },
    {
      "TypeName": "self",
      "Role": "AST Lint Specialist",
      "Model": "flash",
      "Workspace": "inherit",
      "Prompt": "You are a specialized worker in a structural editing swarm. You must resolve the anti-pattern at path [0, 0, 21] in workspace 'worker-branch-2'.\n\nMANDATORY RULES:\n1. Target workspace is 'worker-branch-2'. Every structural-editing call MUST pass \"workspace_id\": \"worker-branch-2\".\n2. DO NOT use text-editing tools. Use ONLY structural-editing MCP tools: ast_modify or ast_replace_pattern.\n3. Report back when the AST modification is complete."
    }
  ]
}
```

### Step 6: Reactive Collect & Merge Gating
When subagents notify you of task completion:
1. **Inspect AST Diff**: Call `workspace_diff`:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_diff",
     "Arguments": {
       "source_workspace_id": "worker-branch-<i>",
       "target_workspace_id": "default"
     }
   }
   ```
2. **Review Safety (`jev_review`)**:
   Pass the AST diff string to `jev_review`. If `safe_to_apply` is low or `blast_radius` is high, reject the branch.
3. **Verify Intent (`jev_verify`)**:
   Pass the assigned task description as the `claim` and the AST diff as `evidence` to `jev_verify`. Ensure result is `verified`.
4. **Merge Branch**: Call `workspace_merge`:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_merge",
     "Arguments": {
       "source_workspace_id": "worker-branch-<i>",
       "target_workspace_id": "default"
     }
   }
   ```

### Step 7: Verification & Disk Persistence
1. Run test suite: execute `./scripts/run-tests.lisp`. All tests must pass (0 failures).
2. Persist in-memory modifications to disk:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "commit_workspace",
     "Arguments": { "workspace_id": "default" }
   }
   ```
3. Loop back to Step 1 for the next pass, or terminate if stopping criteria are met.
