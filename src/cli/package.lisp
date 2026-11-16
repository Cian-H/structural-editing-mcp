(defpackage :structural-editing-mcp.cli
  (:use :cl
        :alexandria
        :structural-editing-mcp.version
        :structural-editing-mcp.conditions
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.workspace
        :structural-editing-mcp.mcp)
  (:import-from :serapeum
    :fmt
    :href
    :string-case
    :string-prefix-p
    :trim-whitespace)
  (:export :run-cli
           :execute-cli
           :execute-cli-in-ram
           :parse-cli-args
           :parse-cli-path
           :find-session-dir
           :save-session
           :load-session
           :run-daemon-foreground
           :daemon-ping
           :daemon-send-request
           :daemon-socket-path)
  (:documentation
    "Command-line interface mirroring and extending the structural-editing MCP tools."))