(defsystem "structural-editing-mcp"
  :version "0.1.0"
  :author "Cian Hughes"
  :license "LGPLv3"
  :depends-on (:trivia :alexandria :serapeum)
  :pathname "src"
  :components ((:file "conditions")
               (:file "utils")
               (:file "tree")
               (:file "parser")
               (:file "edit")
               (:file "workspace")
               (:file "main"))
  :in-order-to ((test-op (test-op "structural-editing-mcp/tests"))))

(defsystem "structural-editing-mcp/tests"
  :depends-on (:structural-editing-mcp
               :rove)
  :pathname "test"
  :components ((:file "package")
               (:file "utils")
               (:file "conditions")
               (:file "tree")
               (:file "parser")
               (:file "edit")
               (:file "workspace"))
  :perform (test-op (o c) (uiop:symbol-call :rove :run c)))
