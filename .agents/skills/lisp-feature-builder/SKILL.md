---
name: lisp-feature-builder
description: >-
  Workflow for safely adding new features to the Lisp codebase using structural-editing MCP tools. Integrates Jev System One semantic navigation, isolated workspace branching, and pre-merge safety reviews.
---

# Lisp Feature Builder

## Overview
This skill provides the standard operating procedure for adding new features or extensions to the `structural-editing-mcp` codebase. It enforces strict structural editing invariants: **no text diffs or direct file edits on Lisp files**. All code additions, symbol exports, and function definitions are executed in memory using the `structural-editing` MCP server, validated through Jev System 1 models, tested, and persisted only when complete.

## Strict Dogfooding Invariants
- **NEVER use text-based tools (`replace_file_content`, text diffs) on Lisp files (`.lisp`, `.cl`, `.asd`).**
- **ALL modifications MUST be performed in-memory via `structural-editing` MCP tools.**
- **Changes persist to disk ONLY via `commit_workspace`.**

## Toolchain & MCP Mappings
- **Structural Editing (`ServerName: "structural-editing"`):**
  - `read_node`: Ingest files into workspace (`load_files`) and view node structures.
  - `workspace_manage`: Branching (`action: "fork"`), file creation (`action: "create_file"`).
  - `workspace_create_file`: Create a new file in memory.
  - `ast_modify`: Insert (`action: "insert"`), overwrite (`action: "overwrite"`), or wrap (`action: "wrap"`).
  - `ast_relocate`: Move (`action: "move"`), copy, swap, or split AST nodes.
  - `workspace_diff`: Compute AST difference between feature branch and `"default"`.
  - `workspace_merge`: Merge feature branch into `"default"`.
  - `commit_workspace`: Persist in-memory workspace changes to disk.
- **System 1 Engine (`ServerName: "jev"`):**
  - `jev_find`: Fast semantic AST navigation to locate insertion points without token bloat.
  - `jev_review`: Review AST diffs for blast radius and safety before merging.

---

## Step-by-Step Execution Workflow

### Step 1: Ingest & Semantically Locate Insertion Points
1. Ensure project files are loaded into `*workspace-tree*`:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "read_node",
     "Arguments": {
       "load_files": ["/home/cianh/Projects/structural-editing-mcp"],
       "mode": "skeleton",
       "path": []
     }
   }
   ```
2. Use the `lisp-jev-ast-navigator` skill (calling `jev_find` on skeleton signatures) to locate the exact AST coordinate where the feature hooks in:
   - Target package definition for exporting symbols (e.g. `[0, 0, 1]` in `src/package.lisp`).
   - Target dispatch table or function list for adding new commands.

### Step 2: Branch the Workspace (`workspace_manage`)
Never develop directly on the `"default"` branch. Fork an isolated branch:
```json
{
  "ServerName": "structural-editing",
  "ToolName": "workspace_manage",
  "Arguments": {
    "action": "fork",
    "source_id": "default",
    "target_id": "feature-<name>"
  }
}
```

### Step 3: Implement the Feature via Structural AST Surgery
Every structural editing call MUST pass `"workspace_id": "feature-<name>"`.

1. **Creating New Files (if applicable):**
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_create_file",
     "Arguments": {
       "file_path": "src/new-feature.lisp",
       "workspace_id": "feature-<name>"
     }
   }
   ```
2. **Inserting New Definitions (Functions, Macros, Variables):**
   Target the enclosing file or form path and call `ast_modify` with `action: "insert"`:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "ast_modify",
     "Arguments": {
       "action": "insert",
       "path": [0, 1, 10],
       "new_node": "(defun my-new-feature (arg) ...)",
       "workspace_id": "feature-<name>"
     }
   }
   ```
3. **Exporting Symbols / Updating Forms:**
   Use `ast_modify` (`action: "insert"` or `action: "overwrite"`) to target the exact parameter list or export list child path.

### Step 4: Self-Audit & Quality Gate (System 1)
1. Run `ast_suggest_refactorings` on the modified path:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "ast_suggest_refactorings",
     "Arguments": {
       "path": [0, 1],
       "workspace_id": "feature-<name>"
     }
   }
   ```
2. Filter any findings through `lisp-jev-refactor-evaluator`. Fix any high-confidence anti-patterns before proceeding.

### Step 5: Run Automated Tests
Execute the Rove test suite:
`./scripts/run-tests.lisp`
All tests must exit with 0 failures.

### Step 6: Diff & Merge Gating (`jev_review`)
1. Compute the AST difference:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_diff",
     "Arguments": {
       "source_workspace_id": "feature-<name>",
       "target_workspace_id": "default"
     }
   }
   ```
2. Call `jev_review` passing the AST diff. Verify that `safe_to_apply` is high and `blast_radius` is bounded.
3. If approved, merge into `"default"`:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_merge",
     "Arguments": {
       "source_workspace_id": "feature-<name>",
       "target_workspace_id": "default"
     }
   }
   ```
4. Persist to disk:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "commit_workspace",
     "Arguments": { "workspace_id": "default" }
   }
   ```
