(defpackage :structural-editing-mcp.mcp
  (:use :cl
        :alexandria
        :trivia
        :structural-editing-mcp.version
        :structural-editing-mcp.tree
        :structural-editing-mcp.parser
        :structural-editing-mcp.edit
        :structural-editing-mcp.analysis
        :structural-editing-mcp.conditions
        :structural-editing-mcp.workspace)
  (:import-from :serapeum
    :dict
    :trim-whitespace
    :ellipsize
    :fmt
    :href
    :defconst
    :string-case)
  (:export :start-server
           :handle-message
           :dict
           :start-worker-pool
           :stop-worker-pool
           :execute-tool-call
           :get-tools-list
           :*mcp-tool-definitions*
           :*mcp-tool-handlers*
           :mutation-tool-p
           :+supported-protocol-versions+
           :+default-legacy-protocol-version+
           :*negotiated-protocol-version*
           :*negotiated-client-capabilities*
           :*request-protocol-version*
           :*request-client-capabilities*
           :*session-initialized-p*
           :mcp-2-0-request-p
           :send-error-with-data
           :send-notification))