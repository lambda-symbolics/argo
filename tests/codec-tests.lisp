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
  (let* ((source  "{\"f\":false,\"t\":true,\"n\":null,\"e\":[],\"o\":{},\"a\":[false,null,1]}")
         (decoded (json-decode source))
         (again   (json-decode (json-encode decoded))))
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
  (let* ((secret    "sk-secret-token")
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
    (dolist (value (list :keyword 'symbol #\a #'first (complex 1 2)
                         (cons 1 2) circular-list cyclic-object
                         (let ((object (make-hash-table)))
                           (setf (gethash 1 object) 2)
                           object)
                         (json-object "nested" (vector #'first))))
      (test-signals json-unrepresentable (json-encode value)))))

(define-test test-shared-values-are-not-cycles
  (let* ((shared (json-array 1))
         (value  (json-array shared shared)))
    (test-equal "[[1],[1]]" (json-encode value))))

(define-test test-ratios-encode-as-floats
  (test-equal 0.5d0 (json-decode (json-encode 1/2)) :test #'=))

(define-test test-codec-marker-options
  (let* ((marker (gensym "FALSE"))
         (source "{\"f\":false,\"n\":null,\"t\":true,\"a\":[false,null,[],{}]}")
         (value  (json-decode source :false-value marker :null-value ':null)))
    (test-assert (eq (gethash "f" value) marker))
    (test-equal ':null (gethash "n" value))
    (test-equal t (gethash "t" value))
    (test-equal (list nil t)
                (multiple-value-list (json-get-present value "f" :false-value marker)))
    (test-equal '(nil nil)
                (multiple-value-list
                 (json-get-present value "missing" :false-value marker)))
    (test-equal source (json-encode value :false-value marker))
    (test-equal source
                (flexi-streams:octets-to-string
                 (json-encode-utf8 value :false-value marker)
                 :external-format ':utf-8))
    (let ((limits (make-json-limits :maximum-nodes 20)))
      (test-equal source (json-encode value :limits limits :false-value marker))
      (test-equal source
                  (flexi-streams:octets-to-string
                   (json-encode-utf8 value :limits limits :false-value marker)
                   :external-format ':utf-8)))
    (test-signals json-limit-exceeded
      (json-decode source :false-value marker
                          :limits      (make-json-limits :maximum-nodes 1)))
    (test-signals json-limit-exceeded
      (json-encode value :false-value marker
                        :limits       (make-json-limits :maximum-characters 1))))
  ;; Markers can themselves be containers. Measurement treats the marker as
  ;; one scalar, just as encoding does, without traversing it.
  (let ((marker (vector 1 2)))
    (test-equal "false" (json-encode marker :false-value marker
                                         :limits         (make-json-limits :maximum-nodes 1)))))

(define-test test-decoding-ignores-ambient-parser-policy
  (let ((yason:*parse-object-as*                ':plist)
        (yason:*parse-object-as-alist*          t)
        (yason:*parse-object-key-fn*            #'string-upcase)
        (yason:*parse-json-arrays-as-vectors*   nil)
        (yason:*parse-json-booleans-as-symbols* nil)
        (yason:*parse-json-null-as-keyword*     t)
        (yason:true                             ':ambient-true)
        (yason:false                            ':ambient-false))
    (let ((value (json-decode "{\"key\":[true,false,null]}")))
      (test-assert (hash-table-p value))
      (test-equal #(t :json-false nil) (gethash "key" value) :test #'equalp)
      (test-equal '(nil nil) (multiple-value-list (gethash "KEY" value)))
      (test-equal "{\"key\":[true,false,null]}" (json-encode value)))))

(define-test test-trailing-data-condition
  (dolist (source '("{} {}" "[] trailing" "true false" "1 2"))
    (test-signals json-trailing-data (json-decode source)))
  (dolist (source '("{\"private\":}" "[1,]" "tru"))
    (let ((condition (test-signals json-syntax-error (json-decode source))))
      (test-assert (not (typep condition 'json-trailing-data))))))


;;;; -- Native Normalization and Formatting --

(define-test test-normalization-markers-and-presence
  (let* ((value (json-object "a" (vector (json-false) nil :null 'yason:false
                                        'yason:true #() (json-object))))
         (copy  (json-normalize value)))
    (test-assert (not (eq value copy)))
    (test-equal #(:json-false nil nil :json-false t #() #())
                (let ((array (gethash "a" copy)))
                  (concatenate 'vector (subseq array 0 6)
                               (vector (make-array (hash-table-count (aref array 6))))))
                :test #'equalp)
    (test-equal '(nil nil) (multiple-value-list (gethash "missing" copy)))
    (dolist (policy (list (list (json-false) nil) (list 'yason:false :null)))
      (destructuring-bind (false null) policy
        (let* ((normalized (json-normalize value :false-value false :null-value null))
               (array      (gethash "a" normalized)))
          (test-assert (eq false (aref array 0)))
          (test-assert (eq false (aref array 3)))
          (test-assert (eq null (aref array 1)))
          (test-assert (eq null (aref array 2)))))))
  (let* ((input  (gensym "INPUT-FALSE"))
         (output (gensym "OUTPUT-FALSE"))
         (copy   (json-normalize (vector input nil) :input-false-value input
                                                     :false-value output :null-value :null)))
    (test-assert (eq output (aref copy 0)))
    (test-equal :null (aref copy 1)))
  (test-equal #(:json-false :null)
              (json-normalize (vector nil :null) :input-false-value nil :null-value :null)
              :test #'equalp))

(define-test test-normalization-independent-occurrences
  (let* ((key        (copy-seq "key"))
         (text       (copy-seq "text"))
         (child      (json-object key (vector text)))
         (source     (vector child child))
         (copy       (json-normalize source))
         (first      (aref copy 0))
         (second     (aref copy 1))
         (first-key  (loop for key being the hash-keys of first return key))
         (second-key (loop for key being the hash-keys of second return key)))
    (test-assert (not (eq first second)))
    (test-assert (not (eq first-key second-key)))
    (test-assert (not (eq key first-key)))
    (test-assert (not (eq (gethash "key" first) (gethash "key" second))))
    (setf (char (aref (gethash "key" first) 0) 0) #\X)
    (test-equal "text" text)
    (test-equal "text" (aref (gethash "key" second) 0))
    (setf (char first-key 0) #\K)
    (test-equal "key" key)
    (test-equal "key" second-key)))

(define-test test-normalization-numeric-codec-parity
  (dolist (value (list 0 -1 (expt 10 100) 1/3 -7/2 0.1f0 -0.0f0 0.1d0 -0.0d0
                       least-positive-single-float least-positive-double-float
                       least-positive-normalized-double-float most-positive-double-float))
    (let ((old (json-decode (json-encode value)))
          (new (json-normalize value)))
      (test-equal old new :test #'eql)
      (when (floatp new)
        (test-assert (typep new 'double-float))
        (test-equal (float-sign old) (float-sign new) :test #'eql))))
  (test-equal #(1 2.5d0) (json-normalize (list 1 5/2)) :test #'equalp))

(define-test test-transforms-reject-invalid-values
  (let ((cycle (vector nil)))
    (setf (aref cycle 0) cycle)
    (dolist (function (list #'json-normalize #'json-encode-sorted #'json-encode-pretty))
      (dolist (value (list cycle (cons 1 2) :invalid #\a (complex 1 2)
                           (let ((table (make-hash-table)))
                             (setf (gethash 1 table) 2)
                             table)))
        (test-signals json-unrepresentable (funcall function value))))))

(define-test test-sorted-encoding-codec-parity
  (labels ((old (value)
             (with-output-to-string (stream)
               (labels ((emit (value)
                          (cond
                            ((hash-table-p value)
                             (write-char #\{ stream)
                             (loop for key in (sort (loop for key being the hash-keys of value
                                                        collect key) #'string<)
                                   for first-p = t then nil
                                   do (unless first-p (write-char #\, stream))
                                      (write-string (json-encode key) stream)
                                      (write-char #\: stream)
                                      (emit (gethash key value)))
                             (write-char #\} stream))
                            ((and (vectorp value) (not (stringp value)))
                             (write-char #\[ stream)
                             (loop for child across value for first-p = t then nil
                                   do (unless first-p (write-char #\, stream)) (emit child))
                             (write-char #\] stream))
                            (t
                             (write-string (json-encode value) stream)))))
                 (emit (json-decode (json-encode value)))))))
    (let ((value (json-object "z" (list 0.1f0 1/3 :null (json-false))
                              "a" (json-object "B" "é😀\"\\" "A" #()) "0" nil)))
      (test-equal (old value) (json-encode-sorted value))
      (test-equal (json-encode-sorted value)
                  (json-encode-sorted
                   (json-object "0" nil "a" (gethash "a" value) "z" (gethash "z" value))))))
  (test-equal "{\"A\":2,\"a\":1,\"z\":[false,null,[]]}"
              (json-encode-sorted (json-object "z" (vector 'yason:false :null #())
                                               "a" 1 "A" 2))))

(define-test test-pretty-encoding-structure-and-strings
  (let ((value (json-object "a" (vector "a,:[{}]\\\"" (json-object) #())
                            "b" (json-false))))
    (test-equal (format nil "{~%  \"a\": [~%    \"a,:[{}]\\\\\\\"\",~%    {},~%    []~%  ],~%  \"b\": false~%}")
                (json-encode-pretty value))
    (test-assert (json-value-equal-p value (json-decode (json-encode-pretty value)))))
  (test-equal "[]" (json-encode-pretty #()))
  (test-equal "{}" (json-encode-pretty (json-object)))
  (test-equal "false" (json-encode-pretty (json-false))))
