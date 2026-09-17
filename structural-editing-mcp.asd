(defsystem "structural-editing-mcp"
  :version "0.1.0"
  :author "Cian Hughes"
  :license "LGPLv3"
  :depends-on (:trivia :alexandria)
  :pathname "src"
  :components ((:file "main")
               (:file "utils")
               (:file "tree")
               (:file "parser")
               (:file "edit")
               (:file "workspace"))
  :in-order-to ((test-op (test-op "structural-editing-mcp/tests"))))

(defsystem "structural-editing-mcp/tests"
  :depends-on (:structural-editing-mcp
               :rove)
  :pathname "test"
  :components ((:file "main"))
  :perform (test-op (o c) (uiop:symbol-call :rove :run c)))
