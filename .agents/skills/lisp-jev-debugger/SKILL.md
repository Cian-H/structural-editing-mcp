---
name: lisp-jev-debugger
description: >-
  Workflow for diagnosing and fixing bugs in the Lisp codebase using structural-editing MCP tools. Uses Jev System One to verify bug-fix claims and isolate blast radius.
---

# Lisp Jev Debugger

## Overview
This skill provides a surgical debugging workflow for fixing defects in the `structural-editing-mcp` project. It enforces strict structural editing invariants: **no text diffs or direct file edits on Lisp files**. The agent locates the bug using `lisp-jev-ast-navigator`, isolates the work in a branched workspace, performs targeted AST surgery at the exact coordinate path, verifies that the diff satisfies the bug report via `jev_verify`, checks safety with `jev_review`, and merges only after tests pass.

## Strict Dogfooding Invariants
- **NEVER use text-based tools (`replace_file_content`, text diffs) on Lisp files (`.lisp`, `.cl`, `.asd`).**
- **ALL modifications MUST be performed in-memory via `structural-editing` MCP tools.**
- **Changes persist to disk ONLY via `commit_workspace`.**

## Toolchain & MCP Mappings
- **Structural Editing (`ServerName: "structural-editing"`):**
  - `read_node` / `read_slice`: Inspect the faulty expression and surrounding AST context.
  - `workspace_manage`: Branching (`action: "fork"`).
  - `ast_modify`: Targeted surgery (`action: "overwrite"`, `action: "insert"`, `action: "wrap"`).
  - `ast_remove`: Delete buggy forms (`action: "delete"`), unwrap (`action: "unwrap"`), promote children (`action: "promote"`).
  - `ast_replace_pattern`: Structural fixes across repeated bug patterns.
  - `workspace_diff`: Compute AST diff of the fix.
  - `workspace_merge`: Merge fix into `"default"`.
  - `commit_workspace`: Persist dirty files to disk.
- **System 1 Engine (`ServerName: "jev"`):**
  - `jev_find`: Locate the offending function or macro.
  - `jev_verify`: Verify bug fix claim against the AST diff.
  - `jev_review`: Ensure the fix has a minimal blast radius.

---

## Step-by-Step Execution Workflow

### Step 1: Locate the Buggy AST Node
1. Ensure the codebase is loaded into `*workspace-tree*` using `read_node(load_files: ["..."])`.
2. Extract key symbols, function names, or error messages from the bug report/stack trace.
3. Use `lisp-jev-ast-navigator` (`jev_find` over skeleton) to locate the exact function path (e.g. `[0, 1, 14]`).
4. Cast a vertical ray using `read_slice` or inspect the node using `read_node` to pinpoint the exact nested expression path needing modification (e.g. `[0, 1, 14, 5, 2]`).

### Step 2: Branch Workspace (`workspace_manage`)
Isolate the bug fix:
```json
{
  "ServerName": "structural-editing",
  "ToolName": "workspace_manage",
  "Arguments": {
    "action": "fork",
    "source_id": "default",
    "target_id": "bugfix-<issue>"
  }
}
```

### Step 3: Perform Surgical AST Fix
Do NOT overwrite the entire parent function. Target the exact sub-expression coordinate:

- **Fixing an incorrect clause / expression:**
  ```json
  {
    "ServerName": "structural-editing",
    "ToolName": "ast_modify",
    "Arguments": {
      "action": "overwrite",
      "path": [0, 1, 14, 5, 2],
      "new_node": "(when (valid-token-p tok) (consume tok))",
      "workspace_id": "bugfix-<issue>"
    }
  }
  ```
- **Removing a broken or redundant form:**
  ```json
  {
    "ServerName": "structural-editing",
    "ToolName": "ast_remove",
    "Arguments": {
      "action": "delete",
      "path": [0, 1, 14, 5, 3],
      "workspace_id": "bugfix-<issue>"
    }
  }
  ```

### Step 4: Intent Verification (`jev_verify`)
1. Obtain the AST diff:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_diff",
     "Arguments": {
       "source_workspace_id": "bugfix-<issue>",
       "target_workspace_id": "default"
     }
   }
   ```
2. Call `jev_verify` with:
   - `claim`: "The AST modification resolves: <user's bug description> without side effects."
   - `evidence`: The AST diff returned by `workspace_diff`.
3. If Jev returns `contradicted` or `unsupported`, do not proceed—re-evaluate the bug cause.

### Step 5: Test Suite Execution
Run the full test suite:
`./scripts/run-tests.lisp`
All tests must pass (0 failures). If the bug fix includes a new regression test, ensure it passes as well.

### Step 6: Blast Radius Review & Merge
1. Pass the AST diff to `jev_review`. A valid bug fix should have a tight, bounded `blast_radius`.
2. Merge into `"default"`:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "workspace_merge",
     "Arguments": {
       "source_workspace_id": "bugfix-<issue>",
       "target_workspace_id": "default"
     }
   }
   ```
3. Persist to disk:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "commit_workspace",
     "Arguments": { "workspace_id": "default" }
   }
   ```
