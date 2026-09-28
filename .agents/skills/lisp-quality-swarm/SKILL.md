---
name: lisp-quality-swarm
description: >-
  Deploys an automated swarm of worker subagents via invoke_subagent to perform code quality and comment curation passes on the Lisp codebase. Uses Jev System One models to filter findings and assess comment value, verify worker intent, and review AST diffs for safety before merging via structural-editing MCP tools.
---

# Lisp Code Quality & Comment Curation Swarm

## Overview
This skill orchestrates a real agent swarm to audit, evaluate, and optimize both code structure and code comments across a Lisp codebase. The calling agent acts as the **Orchestrator**, running static analysis, extracting candidate comments and docstrings, filtering all opportunities with the Jev System One decision engine, and **actually deploying parallel worker subagents via `invoke_subagent`**. 

Each worker operates in an isolated AST branch using the `structural-editing` MCP tools (including surgical comment deletion via `ast_remove`), and changes are strictly gated with `jev_review` and `jev_verify` before being merged and persisted to disk.

## Strict Dogfooding Invariants
- **NEVER use text-based replacement or diff tools (`replace_file_content`, text diffs) on Lisp files (`.lisp`, `.cl`, `.asd`).**
- **ALL modifications (including comment removal/edits) MUST be performed in-memory via `structural-editing` MCP tools.**
- **Changes persist to disk ONLY via `commit_workspace`.**

## Toolchain & MCP Mappings
- **Structural Editing (`ServerName: "structural-editing"`):**
  - `read_node`: Load files into workspace (`load_files`), inspect AST coordinates and `:comment` nodes.
  - `ast_suggest_refactorings`: Aggregate lint, complexity, duplicate, and binding analysis.
  - `ast_remove`: Surgically delete dead code or useless comments (`action: "delete"`).
  - `ast_modify`: Rewrite expressions or tighten comments (`action: "overwrite"`, `action: "insert"`).
  - `workspace_manage`: Manage lifecycle (`action: "fork"` to branch, `action: "list"` to inspect).
  - `workspace_diff`: Compute AST diff between worker branch and `"default"`.
  - `workspace_merge`: Merge worker branch into `"default"`.
  - `commit_workspace`: Persist dirty files to disk.
- **System 1 Engine (`ServerName: "jev"`):**
  - `jev_decide`: Filter structural findings and assess comment signal-to-noise ratio via `lisp-jev-refactor-evaluator`.
  - `jev_review`: Review AST diffs for `safe_to_apply` and `blast_radius`.
  - `jev_verify`: Verify worker changes match the assigned refactoring/comment curation intent.
- **Subagent Swarm Management:**
  - `invoke_subagent`: Launch worker subagents concurrently.

---

## Orchestrator Execution Workflow

### Step 1: Workspace Ingestion & Comprehensive Audit
1. Call `read_node` (with `load_files: ["/path/to/project"]` or target file) to ensure all source files are loaded into `*workspace-tree*`.
2. **Structural Audit**: Call `ast_suggest_refactorings` on target path with `min_priority: "style"`.
3. **Comment Audit**: Inspect the target forms with `read_node(depth: 2)` to discover `:comment` AST nodes and top-level docstrings.

### Step 2: System 1 Filtration via Jev (`jev_decide`)
Use `lisp-jev-refactor-evaluator` rules:
1. **Structural Findings**: Pass each finding to `jev_decide`. Approve only if `confidence >= 0.68` AND `selected == "apply"`.
2. **Comment Evaluation**: Pass each comment and its adjacent code to `jev_decide`:
   - Good comments explain *why* (intent, rationale, invariants).
   - Bad comments merely paraphrase *what* the code obviously does (e.g. `;; set x to 0` before `(setf x 0)`).
   - If Jev selects `remove` with `confidence >= 0.68`: **Approve for comment removal**.
   - If Jev selects `rewrite` with `confidence >= 0.68`: **Approve for comment rewriting**.
   - Otherwise: Leave comment untouched.

### Step 3: Termination Check (System 2)
- If 0 approved structural findings AND 0 approved comment cleanups remain: Terminate the swarm, present a summary of work completed, and stop.
- If approved tasks exist, proceed to Step 4.

### Step 4: Branch Isolation (`workspace_manage`)
For each approved task `i` with target coordinate `path`:
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
Deploy worker subagents concurrently using the `invoke_subagent` tool. Group approved delegations into a single call, including the specialized `Comment_Curation_Agent`:

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
      "Role": "Comment Curation Specialist",
      "Model": "flash",
      "Workspace": "inherit",
      "Prompt": "You are a specialized worker in a structural editing swarm. The comment at path [0, 0, 18, 2] was determined by Jev System 1 to be a redundant, verbose duplication of the code.\n\nMANDATORY RULES:\n1. Target workspace is 'worker-branch-2'. Every structural-editing call MUST pass \"workspace_id\": \"worker-branch-2\".\n2. DO NOT use text-editing tools. Use ONLY structural-editing MCP tools: call ast_remove with \"action\": \"delete\" at path [0, 0, 18, 2] to eliminate the useless comment cleanly without affecting AST parenthesis balance.\n3. Report back when the comment node is removed."
    }
  ]
}
```

### Step 6: Reactive Collect & Merge Gating
When subagents notify you of task completion:
1. **Inspect AST Diff**: Call `workspace_diff` between `"worker-branch-<i>"` and `"default"`.
2. **Review Safety (`jev_review`)**:
   Pass the AST diff string to `jev_review`. If `safe_to_apply` is low or `blast_radius` is high, reject the branch.
3. **Verify Intent (`jev_verify`)**:
   Pass the assigned task description as the `claim` and the AST diff as `evidence` to `jev_verify`. Ensure result is `verified`.
4. **Merge Branch**: Call `workspace_merge(source_workspace_id: "worker-branch-<i>", target_workspace_id: "default")`.

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
