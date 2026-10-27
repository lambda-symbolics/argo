(asdf:defsystem #:argo
  :description "A JSON value model for Common Lisp that keeps false, null, and empty arrays apart."
  :author      "Lambda Symbolics OÜ"
  :license     "COLL-Attribution"
  :version     "0.1.0"
  :serial      t
  :depends-on  (#:flexi-streams
                #:serapeum
                #:yason)
  :components  ((:module     "src"
                 :serial     t
                 :components ((:file "package")
                              (:file "conditions")
                              (:file "values")
                              (:file "limits")
                              (:file "grammar")
                              (:file "codec"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:argo/tests))))

(asdf:defsystem #:argo/tests
  :description "Tests for argo."
  :depends-on  (#:argo)
  :serial      t
  :components  ((:module     "tests"
                 :serial     t
                 :components ((:file "test-support")
                              (:file "value-tests")
                              (:file "codec-tests")
                              (:file "limit-tests")
                              (:file "tests"))))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (uiop:symbol-call '#:argo '#:run-tests)))
