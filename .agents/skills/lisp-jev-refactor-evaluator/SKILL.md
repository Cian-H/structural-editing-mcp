---
name: lisp-jev-refactor-evaluator
description: >-
  Evaluates structural-editing-mcp static analysis findings and code comment/docstring quality using the Jev System One decision model. Filters out unsafe refactorings and useless/redundant comments, keeping the context window focused.
---

# Lisp Jev Refactor & Comment Evaluator

## Overview
This skill orchestrates the interaction between `structural-editing-mcp` and the `jev` MCP. It takes two forms of code-quality candidates:
1. **Raw structural static analysis findings** (lint anti-patterns, cyclomatic complexity, code duplicates, lexical binding issues).
2. **Code comments and docstrings** (assessing whether comments add genuine explanatory value vs. being verbose, redundant duplication of the code).

It formats each candidate for the Jev System One decision engine (`jev_decide`), evaluates the results, and filters out noise. Findings with confidence below 1-sigma (`< 0.68`) are silently discarded, while high-confidence actionable verdicts are passed to the agent or swarm workers.

## Dependencies
- **structural-editing-mcp**: Required to read the AST, inspect comment nodes, and generate static analysis (`ast_suggest_refactorings`, `read_node`, `read_slice`).
- **jev**: Required to evaluate safety, value, and signal-to-noise ratio (`jev_decide`).

---

## Part A: Evaluating Structural Refactorings

### 1. Extract Findings
Call `ast_suggest_refactorings` for the target path:
```json
{
  "ServerName": "structural-editing",
  "ToolName": "ast_suggest_refactorings",
  "Arguments": { "path": [0, 0], "min_priority": "style" }
}
```

### 2. Formulate Refactoring Jev Request
For each finding, format a `jev_decide` request:
- **decision**: "Should we apply this structural refactoring to the Lisp AST?"
- **evidence**: The specific AST node rendered code combined with the finding category, message, and suggested fix.
- **priorities**: "Prioritize parenthetical integrity, idiomatic Lisp style, and keeping churn low. Avoid unnecessary refactoring if the code clone is a trivial idiom or raw complexity is in a cohesive loop."
- **candidates**:
  - `id: "apply"`, `description: "The refactoring is safe, idiomatic, and highly valuable."`
  - `id: "ignore"`, `description: "The finding is a false positive or not worth the risk/churn."`

### 3. Filter by Confidence
- If `confidence < 0.68` OR `selected == "ignore"`: **Silently discard**.
- If `confidence >= 0.68` AND `selected == "apply"`: **Execute or delegate**.

---

## Part B: Assessing Comment & Docstring Value

Lisp AST represents comments as first-class `:comment` nodes and docstrings as leaf strings. Comments must have a high signal-to-noise ratio:
- **Good Comments**: Explain *why* (architectural invariants, non-obvious design trade-offs, bug workarounds, external constraints).
- **Bad Comments**: Paraphrase *what* the code already expresses directly, restate variable names, or duplicate obvious language syntax (e.g., `;; increment counter by 1` before `(incf counter)`).

### 1. Extract Comment & Context
Find the comment AST node (tag `:comment` or docstring leaf) and retrieve the immediately adjacent or enclosing expression code:
```json
{
  "ServerName": "structural-editing",
  "ToolName": "read_node",
  "Arguments": { "path": [0, 0, 15], "depth": 2 }
}
```

### 2. Formulate Comment Jev Request
Query `jev_decide` with the comment and its adjacent code:
```json
{
  "ServerName": "jev",
  "ToolName": "jev_decide",
  "Arguments": {
    "decision": "Assess the value of this code comment. Should it be kept, removed, or rewritten?",
    "evidence": "Comment text:\n\";; <comment text>\"\n\nAssociated code:\n(<adjacent form>)",
    "priorities": "Prioritize high signal-to-noise ratio. Good comments explain 'why' (intent, rationale, trade-offs, invariants). Bad comments duplicate 'what' the code obviously does. Ruthlessly eliminate redundant comments that merely paraphrase standard language constructs or variable names.",
    "candidates": [
      {
        "id": "keep",
        "description": "The comment adds genuine value: it explains non-obvious rationale, architectural invariants, or tricky domain 'why' that cannot be inferred from reading the code directly. Keep it."
      },
      {
        "id": "remove",
        "description": "The comment is low-value noise: it merely restates or paraphrases what the code is already doing, duplicates obvious syntax, or is redundant clutter. Remove it."
      },
      {
        "id": "rewrite",
        "description": "The comment has a useful kernel of intent but is overly verbose, awkwardly phrased, or outdated. Rewrite it concisely."
      }
    ]
  }
}
```

### 3. Filter & Action Verdict
- **`confidence < 0.68`**: Silently discard (leave comment untouched).
- **`selected == "keep"`**: Leave comment untouched.
- **`selected == "remove"` (with `confidence >= 0.68`)**:
  Remove the comment node cleanly via `ast_remove`:
  ```json
  {
    "ServerName": "structural-editing",
    "ToolName": "ast_remove",
    "Arguments": {
      "action": "delete",
      "path": [0, 0, 15, 2],
      "workspace_id": "<workspace_id>"
    }
  }
  ```
- **`selected == "rewrite"` (with `confidence >= 0.68`)**:
  Overwrite the comment node with a tight, concise rationale via `ast_modify`:
  ```json
  {
    "ServerName": "structural-editing",
    "ToolName": "ast_modify",
    "Arguments": {
      "action": "overwrite",
      "path": [0, 0, 15, 2],
      "new_node": ";; <tight rationale>",
      "workspace_id": "<workspace_id>"
    }
  }
  ```

---

## Rate Limiting & Errors
If Jev returns a rate-limit or temporary server error, apply exponential backoff (retrying up to 3 times: 1s, 2s, 4s). If an error persists after 3 retries, fail loudly.
