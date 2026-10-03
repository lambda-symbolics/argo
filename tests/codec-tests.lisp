(in-package #:argo)

;;;; -- Codec Tests --

(define-test test-encoding-table
  (dolist (case (list (list nil "null")
                      (list :null "null")
                      (list t "true")
                      (list (json-false) "false")
                      (list 'yason:false "false")
                      (list (json-array) "[]")
                      (list (list 1 2) "[1,2]")
                      (list (vector "a" nil t) "[\"a\",null,true]")
                      (list "žluť \"q\"" "\"žluť \\\"q\\\"\"")
                      (list 42 "42")))
    (destructuring-bind (value expected) case
      (test-equal expected (json-encode value)))))

(define-test test-round-trip-keeps-false-null-and-empty-arrays
  (let* ((source "{\"f\":false,\"t\":true,\"n\":null,\"e\":[],\"o\":{},\"a\":[false,null,1]}")
         (decoded (json-decode source))
         (again (json-decode (json-encode decoded))))
    (dolist (value (list decoded again))
      (test-assert (json-false-p (gethash "f" value)))
      (test-equal t (json-get value "t"))
      (multiple-value-bind (null-value present-p) (json-get-present value "n")
        (test-assert (and (null null-value) present-p)))
      (test-assert (and (vectorp (json-get value "e")) (zerop (length (json-get value "e")))))
      (test-assert (and (json-object-p (json-get value "o"))
                        (zerop (hash-table-count (json-get value "o")))))
      (let ((array (json-get value "a")))
        (test-assert (json-false-p (aref array 0)))
        (test-equal nil (aref array 1))
        (test-equal 1 (aref array 2))))))

(define-test test-encoding-does-not-modify-values
  (let ((object (json-object "f" (json-false))))
    (json-encode object)
    (test-equal :json-false (gethash "f" object))))

(define-test test-utf8-encoding
  (let ((octets (json-encode-utf8 (json-object "text" "příliš"))))
    (test-equal "{\"text\":\"příliš\"}"
                (flexi-streams:octets-to-string octets :external-format :utf-8))))

(define-test test-decoding-shapes
  (let ((array (json-decode "[1,2,3]")))
    (test-assert (and (vectorp array) (not (stringp array))))
    (test-equal '(1 2 3) (coerce array 'list)))
  (test-equal 1.5d0 (json-decode " 1.5 ") :test #'=)
  (test-equal -12.5d2 (json-decode "-12.5E+2") :test #'=)
  (test-equal (format nil "x~C~C" #\Newline (code-char #xE9))
              (json-decode "\"x\\n\\u00e9\"")))

(define-test test-grammar-accepts-rfc-8259-documents
  (dolist (source '("0" "-0" "1e5" "0.25E-3" "\"\"" "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\""
                    "true" "false" "null" "[]" "{}" " [ 1 , { \"a\" : [ ] } ] "
                    "{\"a\":{\"b\":[null,true,false,\"c\"]}}"))
    (test-assert (handler-case
                     (progn
                       (json-decode source)
                       t)
                   (json-error ()
                     nil))
                 source)))

(define-test test-grammar-rejects-malformed-documents
  (dolist (case '(("" 0) ("{" 1) ("[1,]" 3) ("{\"a\":1,}" 7) ("{1:2}" 1)
                  ("{a:1}" 1) ("[1] [2]" 4) ("{\"a\":1} x" 8) ("nul" 0)
                  ("01" 1) ("1." 2) (".5" 0) ("1e" 2) ("+1" 0) ("-" 1)
                  ("\"abc" 4) ("\"\\x\"" 2) ("\"\\u12g4\"" 5)
                  ("[1 2]" 3) ("{\"a\" 1}" 5) ("[true false]" 6)))
    (destructuring-bind (source position) case
      (let ((condition (test-signals json-syntax-error (json-decode source))))
        (test-equal position (json-syntax-error-position condition))))))

(define-test test-syntax-errors-do-not-quote-the-source
  (let* ((secret "sk-secret-token")
         (condition (test-signals json-syntax-error
                      (json-decode (format nil "{\"key\":\"~A\",}" secret)))))
    (test-assert (null (search secret (princ-to-string condition))))))

(define-test test-unescaped-control-characters-are-rejected
  (test-signals json-syntax-error
    (json-decode (format nil "\"a~Cb\"" (code-char 1)))))

(define-test test-object-source-predicate
  (test-assert (json-object-source-p " {\"a\":[1]} "))
  (test-assert (not (json-object-source-p "[1]")))
  (test-assert (not (json-object-source-p "{} {}")))
  (test-assert (not (json-object-source-p "{\"a\":1,}")))
  (test-assert (not (json-object-source-p 42))))

(define-test test-unrepresentable-values
  (let ((cyclic-object (json-object))
        (circular-list (list 1 2)))
    (setf (gethash "self" cyclic-object) cyclic-object)
    (setf (rest (rest circular-list)) circular-list)
    (dolist (value (list :keyword 'symbol #\a #'car (complex 1 2)
                         (cons 1 2) circular-list cyclic-object
                         (let ((object (make-hash-table)))
                           (setf (gethash 1 object) 2)
                           object)
                         (json-object "nested" (vector #'car))))
      (test-signals json-unrepresentable (json-encode value)))))

(define-test test-shared-values-are-not-cycles
  (let* ((shared (json-array 1))
         (value (json-array shared shared)))
    (test-equal "[[1],[1]]" (json-encode value))))

(define-test test-ratios-encode-as-floats
  (test-equal 0.5d0 (json-decode (json-encode 1/2)) :test #'=))
