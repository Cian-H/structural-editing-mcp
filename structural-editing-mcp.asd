(defsystem "structural-editing-mcp"
  :version (:read-file-line "version.txt")
  :author "Cian Hughes"
  :license "LGPLv3"
  :depends-on (:trivia :alexandria :serapeum :yason :cl-indentify :bordeaux-threads)
  :pathname "src"
  :components ((:file "conditions")
               (:file "version")
               (:file "tree")
               (:file "parser")
               (:file "edit")
               (:file "analysis")
               (:file "refactor")
               (:file "workspace")
               (:file "mcp")
               (:file "main"))
  :in-order-to ((test-op (test-op "structural-editing-mcp/tests"))))

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