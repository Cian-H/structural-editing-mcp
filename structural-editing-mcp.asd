(defsystem "structural-editing-mcp" :version
                                    (:read-file-line "version.txt")
                                    :author
                                    "Cian Hughes"
                                    :license
                                    "LGPLv3"
                                    :depends-on
                                    (:trivia :alexandria :serapeum :yason :cl-indentify :bordeaux-threads)
                                    :pathname
                                    "src"
                                    :components
                                    ((:file "conditions") (:file "version")
                                     (:file "tree")
                                     (:file "parser")
                                     (:file "edit")
                                     (:module "analysis"
                                              :serial
                                              t
                                              :components
                                              ((:file "package") (:file "common")
                                               (:file "patterns")
                                               (:file "lint")
                                               (:file "complexity")
                                               (:file "duplicates")
                                               (:file "bindings")
                                               (:file "suggestions")))
                                     (:file "refactor")
                                     (:file "workspace")
                                     (:module "mcp"
                                              :serial
                                              t
                                              :components
                                              ((:file "package") (:file "protocol")
                                               (:file "preview")
                                               (:file "core")
                                               (:file "tools")
                                               (:file "server")))
                                     (:file "main"))
                                    :in-order-to
                                    ((test-op (test-op "structural-editing-mcp/tests"))))

(defsystem "structural-editing-mcp/tests"
  :depends-on
  (:structural-editing-mcp :rove)
  :pathname
  "test"
  :components
  ((:file "package") (:file "version")
   (:file "conditions")
   (:file "tree")
   (:file "parser")
   (:file "edit")
   (:file "analysis")
   (:file "workspace")
   (:file "mcp"))
  :perform
  (test-op (o c) (uiop:symbol-call :rove :run c :style :dot)))