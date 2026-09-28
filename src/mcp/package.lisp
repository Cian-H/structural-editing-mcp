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
  (:import-from :serapeum :dict :trim-whitespace :ellipsize :fmt :href :defconst :string-case)
  (:export :start-server :handle-message :dict :start-worker-pool :stop-worker-pool))
