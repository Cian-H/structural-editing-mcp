---
name: mcp-server-dev
description: Guide for exposing, modifying, and testing MCP tools and protocol handlers in structural-editing-mcp.
---

# MCP Server Development Skill

This skill explains how to add or modify Model Context Protocol (MCP) tools in [`src/mcp.lisp`](file:///home/cianh/Projects/structural-editing-mcp/src/mcp.lisp) and verify their JSON-RPC behavior.

---

## 1. Protocol Architecture

The server implements the Model Context Protocol over JSON-RPC 2.0 via standard I/O (`*standard-input*` / `*standard-output*`).

Key components in `src/mcp.lisp`:
- `start-server`: Main read-eval loop reading JSON-RPC lines from `*standard-input*`.
- `handle-message`: Dispatches incoming JSON-RPC methods (`initialize`, `tools/list`, `tools/call`).
- `send-result`: Encodes and flushes JSON response to `*standard-output*`.
- `send-error`: Encodes standard JSON-RPC error response.

---

## 2. Steps to Expose a New Tool

### Step 1: Register Tool in `tools/list`
Add the tool's JSON schema descriptor to the tools array in the `tools/list` handler:
```lisp
(dict "name" "my_tool_name"
      "description" "Detailed description of what the tool accomplishes."
      "inputSchema" (dict "type" "object"
                          "properties" (dict "path" (dict "type" "array"
                                                          "items" (dict "type" "integer")
                                                          "description" "Integer path coordinates.")
                                             "option_flag" (dict "type" "boolean"
                                                                 "description" "Optional flag."))
                          "required" (vector "path")))
```

### Step 2: Handle Tool Dispatch in `tools/call`
Add a branch for `"my_tool_name"` inside the `tools/call` method dispatch in `handle-message`:
```lisp
((string= name "my_tool_name")
 (let* ((path (to-list (gethash "path" args)))
        (flag (gethash "option_flag" args)))
   (unless path
     (error 'structural-editing-mcp.conditions:missing-argument-error :argument "path"))
   (let ((result (execute-my-tool path :flag flag)))
     (send-result id (dict "content" (vector (dict "type" "text"
                                                   "text" result)))))))
```

### Step 3: Ensure Safe Error Handling
Wrap tool execution in condition handlers to return well-formed JSON-RPC error codes instead of crashing:
```lisp
(handler-case
    (dispatch-tool-call name args id)
  (structural-editing-mcp.conditions:mcp-error (c)
    (send-error id -32602 (format nil "~A" c)))
  (error (c)
    (send-error id -32603 (format nil "Internal server error: ~A" c))))
```

---

## 3. Critical Stdio Rules

- **NEVER** write any raw text to `*standard-output*`.
- Only `send-json` (using `yason:encode`) should write to `*standard-output*`.
- Use `*error-output*` for server logging:
  ```lisp
  (format *error-output* "~&[SERVER LOG] Executing tool ~A~%" name)
  ```

---

## 4. Verifying MCP Endpoints

### 1. Automated Test in `test/mcp.lisp`
Add a test in `test/mcp.lisp` checking that `handle-message` returns the expected JSON response:
```lisp
(deftest test-my-tool-mcp
  (testing "my_tool returns expected text"
    ;; ...
    ))
```
Run tests:
```bash
./scripts/run-tests.lisp test-mcp-tools-list
```

### 2. Manual Stdio JSON-RPC Ping
Test the server directly from the terminal:
```bash
./scripts/run-server.lisp <<< '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```
Verify that the output is pure JSON and parses cleanly.
