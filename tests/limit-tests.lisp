(in-package #:argo)

;;;; -- Limit Tests --

(define-test test-decoding-limits
  (dolist (case (list (list (make-json-limits :maximum-depth 2) "[[[1]]]" :depth)
                      (list (make-json-limits :maximum-nodes 3) "[1,2,3]" :nodes)
                      (list (make-json-limits :maximum-string-characters 3)
                            "[\"abcd\"]" :string-characters)
                      (list (make-json-limits :maximum-aggregate-string-characters 5)
                            "[\"abc\",\"def\"]" :aggregate-string-characters)
                      (list (make-json-limits :maximum-object-key-characters 2)
                            "{\"abc\":1}" :object-key-characters)
                      (list (make-json-limits :maximum-object-members 1)
                            "{\"a\":1,\"b\":2}" :object-members)
                      (list (make-json-limits :maximum-array-elements 2)
                            "[1,2,3]" :array-elements)
                      (list (make-json-limits :maximum-number-characters 3)
                            "[12345]" :number-characters)
                      (list (make-json-limits :maximum-characters 4) "[1,2]" :characters)))
    (destructuring-bind (limits source constraint) case
      (let ((condition (test-signals json-limit-exceeded (json-decode source :limits limits))))
        (test-equal constraint (json-limit-exceeded-constraint condition))))))

(define-test test-limits-allow-conforming-documents
  (test-equal 3 (length (json-decode "[1,2,3]" :limits *json-strict-limits*)))
  (test-equal "[1,2,3]" (json-encode (json-array 1 2 3) :limits *json-strict-limits*)))

(define-test test-deep-documents-are-refused-without-recursion
  (let ((source (concatenate 'string
                             (make-string 100000 :initial-element #\[)
                             (make-string 100000 :initial-element #\]))))
    (test-signals json-limit-exceeded (json-decode source :limits *json-strict-limits*))))

(define-test test-encoding-limits
  (test-signals json-limit-exceeded
    (json-encode (json-array 1 2 3) :limits (make-json-limits :maximum-array-elements 2)))
  (test-signals json-limit-exceeded
    (json-encode "abcdef" :limits (make-json-limits :maximum-characters 4)))
  (test-signals json-limit-exceeded
    (json-encode 1/3 :limits (make-json-limits :maximum-number-characters 5))))
