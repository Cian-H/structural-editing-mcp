---
name: lisp-jev-ast-navigator
description: >-
  Uses Jev System One semantic search (jev_find) to instantly locate the exact AST coordinate path for a natural language query, avoiding context window blowout from reading massive skeleton trees.
---

# Lisp Jev AST Navigator

## Overview
Scanning large Lisp files or workspaces for a specific function, macro, or logic block traditionally requires dumping massive AST skeleton trees into the context window and using heavy System 2 reasoning to guess the right path. This skill eliminates that bottleneck by offloading the search to the `jev_find` System 1 model. It maps AST skeleton signatures from `structural-editing-mcp` into candidates and performs an instant parallel semantic search to return the exact coordinate path.

## Dependencies
- **structural-editing-mcp**: Required to read the AST skeleton (`read_node`) and vertical rays (`read_slice`).
- **jev**: The TypeSafe Jev MCP server is required for parallel semantic search (`jev_find`).

## Quick Start
When a user asks you to "Find the function that handles X" or "Navigate to the logic for Y", use the workflow below to instantly ray-cast to the correct AST node.

## Workflow

### 1. Ingest & Fetch the Skeleton Tree
Call `read_node` with `mode: "skeleton"` on the target file or workspace root:
```json
{
  "ServerName": "structural-editing",
  "ToolName": "read_node",
  "Arguments": {
    "load_files": ["/home/cianh/Projects/structural-editing-mcp/src/analysis.lisp"],
    "path": [0, 0],
    "mode": "skeleton",
    "limit": 250
  }
}
```
**CRITICAL**: You must set `mode: "skeleton"` and ensure you are looking at the top-level form signatures. Do not fetch full AST code bodies for the entire file.

### 2. Formulate the Jev Request
Extract the child nodes and their integer paths from the skeleton output, and map them into the `candidates` array required by `jev_find` (up to 250 candidates):
- `id`: The stringified AST path (e.g. `"[0, 0, 14]"`).
- `text`: The rendered skeleton signature (e.g. `"(defun parse-bindings (body scope) ...)"`).

Set the `query` field to your natural language target (e.g. "The function that analyzes unused lexical bindings").

### 3. Query Jev (`jev_find`)
Call `jev_find`:
```json
{
  "ServerName": "jev",
  "ToolName": "jev_find",
  "Arguments": {
    "query": "function analyzing unused lexical bindings",
    "candidates": [
      { "id": "[0, 0, 14]", "text": "(defun check-unused-in-scope (scope dialect findings-acc) ...)" },
      { "id": "[0, 0, 15]", "text": "(defun extract-let-clauses (node dialect) ...)" }
    ],
    "top_k": 1
  }
}
```

### 4. Direct Access & Verification
1. Take the top-ranked candidate's `id` (e.g. `"[0, 0, 14]"`), and parse it into an integer path: `[0, 0, 14]`.
2. Cast a vertical ray or view the exact form using `read_node`:
   ```json
   {
     "ServerName": "structural-editing",
     "ToolName": "read_node",
     "Arguments": {
       "path": [0, 0, 14],
       "mode": "full",
       "depth": 2
     }
   }
   ```
3. If Jev indicates no candidates match the query, inform the user or search a different file.

## Common Mistakes
- **Passing Full Nodes**: Do not pass full multi-line code bodies as candidates. Keep candidates as single-line skeleton signatures to stay within Jev's character limits and save bandwidth.
- **Sequential Probing**: Do not query candidates one by one. Pass all signatures in a single parallel `jev_find` call.
