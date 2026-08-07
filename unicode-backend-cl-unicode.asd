(defsystem "unicode-backend-cl-unicode"
  :version "0.1.0"
  :description "unicode-protocol backend over cl-unicode (properties, normalize, case, IDNA)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("unicode-protocol" "cl-unicode")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "util")
               (:file "backend")
               (:file "properties")
               (:file "normalize")
               (:file "case")
               (:file "idna"))
  :in-order-to ((test-op (test-op "unicode-backend-cl-unicode/tests"))))

(defsystem "unicode-backend-cl-unicode/tests"
  :depends-on ("unicode-backend-cl-unicode" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "backend-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
