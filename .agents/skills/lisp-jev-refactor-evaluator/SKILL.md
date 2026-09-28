---
name: lisp-jev-refactor-evaluator
description: >-
  Evaluates structural-editing-mcp static analysis findings using the Jev System One decision model. Filters out unsafe or low-value refactorings for Lisp codebases, keeping the context window focused.
---

# Lisp Jev Refactor Evaluator

## Overview
This skill orchestrates the interaction between the `structural-editing-mcp` and the `jev` MCP. It takes raw static analysis findings (like lint warnings, complexity issues, and shadowed bindings), formats them for the Jev model (System One), and filters the results. This allows agents to make fast, reliable code-quality decisions without wasting System 2 tokens or context space on low-confidence suggestions.

## Dependencies
- **structural-editing-mcp**: Required to read the AST and generate static analysis (`ast_suggest_refactorings`, `read_node`).
- **jev**: The TypeSafe Jev MCP server is required to evaluate the safety and value of the findings (specifically `jev_decide`).

## Quick Start
When a user asks you to "evaluate refactorings for `src/analysis.lisp`", follow the workflow steps below manually.

## Workflow

### 1. Extract Findings
- Call `ast_suggest_refactorings` (or `ast_lint`) from the `structural-editing-mcp` for the requested node or file.
- Keep the AST contents of the target node in context.

### 2. Formulate the Jev Request
- For each finding, prepare a request for the `jev_decide` tool.
- **decision**: "Should we apply this structural refactoring to the Lisp AST?"
- **evidence**: The exact AST node snippet combined with the specific static analysis finding, message, and suggested fix.
- **priorities**: "Prioritize parenthetical integrity, idiomatic Lisp style, and keeping churn low. Avoid unnecessary refactoring."
- **candidates**: Provide at least two candidates:
  - `id: "apply"`, `description: "The refactoring is safe, idiomatic, and highly valuable."`
  - `id: "ignore"`, `description: "The finding is a false positive or not worth the risk/churn."`

### 3. Query Jev with Backoff
- Call the `jev_decide` tool.
- If the Jev MCP returns an error (e.g., rate limit or overload), **retry up to 3 times** using exponential backoff.
- If the error persists after 3 retries, fail loudly and inform the user.

### 4. Filter by Confidence
- Examine Jev's `confidence` score in the response.
- If `confidence < 0.68` (1 sigma), **silently discard** the finding. Do not present it to the user.
- If `confidence >= 0.68`, proceed based on the chosen candidate (`apply` or `ignore`).

### 5. Execute or Present
- For the approved, high-confidence refactorings (`apply`), use the structural editing tools (`ast_modify`, `ast_replace_pattern`, etc.) to execute the changes, or present them to the user.
- Do not mention the discarded or ignored findings.

## Rate Limiting
Because this is a manual orchestration loop, you (the agent) are responsible for rate limits. If Jev returns an overload error, wait briefly before retrying. 

## Common Mistakes
- **Forgetting Confidence Thresholds**: Failing to drop decisions with `< 0.68` confidence, resulting in a noisy context window.
- **Passing Too Much State**: Sending the entire file as evidence instead of just the relevant AST node and finding. Keep the `evidence` tightly scoped to the specific expression being evaluated.
