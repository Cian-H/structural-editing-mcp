---
name: lisp-jev-ast-navigator
description: >-
  Uses Jev System One semantic search (jev_find) to instantly locate the exact AST coordinate path for a natural language query, avoiding context window blowout from reading massive skeleton trees.
---

# Lisp Jev AST Navigator

## Overview
Scanning large Lisp files or workspaces for a specific function, macro, or logic block traditionally requires dumping massive AST skeleton trees into the context window and using heavy System 2 reasoning to guess the right path. This skill eliminates that bottleneck by offloading the search to the `jev_find` System 1 model. It maps AST skeleton signatures into candidates and performs an instant parallel semantic search to return the exact coordinate path.

## Dependencies
- **structural-editing-mcp**: Required to read the AST skeleton (`read_node`).
- **jev**: The TypeSafe Jev MCP server is required for parallel semantic search (`jev_find`).

## Quick Start
When a user asks you to "Find the function that handles X" or "Navigate to the logic for Y", use the workflow below to instantly ray-cast to the correct AST node.

## Workflow

### 1. Fetch the Skeleton Tree
- Call the `read_node` tool from `structural-editing-mcp` on the target file or workspace root.
- **CRITICAL**: You must set `mode: "skeleton"` and ensure you are looking at the top-level forms. Do not fetch the full AST body for the entire file.

### 2. Formulate the Jev Request
- Extract the list of child nodes and their integer paths from the skeleton output.
- Map them into the `candidates` array format required by `jev_find` (up to 250 candidates):
  - `id`: A stringified version of the exact AST path (e.g., `"[0, 0, 14]"`).
  - `text`: The rendered skeleton signature of the node (e.g., `"(defun parse-bindings ...)"`).
- Set the `query` field to the natural language description of what you are looking for (e.g., "The function that analyzes unused lexical bindings").

### 3. Query Jev (`jev_find`)
- Call the `jev_find` tool with your formulated `query` and `candidates` array.
- Jev will return the top-ranked candidate(s). Note that `jev_find` also runs a Noul check internally to ensure the top candidate actually answers the query (preventing hallucinations).

### 4. Direct Access & Verification
- If Jev successfully finds a hit, take the returned `id` (the stringified AST path) and parse it back into an integer array (e.g., `[0, 0, 14]`).
- Call `read_node` (with `mode: "full"`) or `read_slice` on that exact integer path to immediately access the target code and continue your task.
- If Jev indicates no candidate matches the query, inform the user or try a different file.

## Common Mistakes
- **Passing Full Nodes**: Do not pass the full rendered code of nodes as candidates. Stick to the surface-level signatures provided by `mode: "skeleton"` to prevent exceeding Jev's 2000-character per-candidate limit.
- **Sequential Probing**: Do not use `jev_decide` to evaluate candidates one by one. Always use `jev_find` to evaluate up to 250 candidates simultaneously in a single parallel request.
