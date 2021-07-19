(defsystem "cl-warehouse-test"
  :defsystem-depends-on ("prove-asdf")
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("cl-warehouse"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-warehouse"))))
  :description "Test system for cl-warehouse"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
