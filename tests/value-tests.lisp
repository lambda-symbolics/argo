(in-package #:argo)

;;;; -- Value Model Tests --

(define-test test-objects-and-access
  (let ((object (json-object "a" 1 "b" nil "c" (json-false))))
    (test-assert (json-object-p object))
    (test-equal 1 (json-get object "a"))
    (test-equal nil (json-get object "b" :absent))
    (test-equal nil (json-get object "c" :absent)
                :test #'eq)
    (test-equal :absent (json-get object "d" :absent))
    (multiple-value-bind (value present-p) (json-get-present object "c")
      (test-assert (and (null value) present-p)
                   "false reads as NIL but is present"))
    (test-assert (json-false-p (gethash "c" object))
                 "the false marker stays in the object")
    (let ((copy (json-object-copy object)))
      (setf (gethash "a" copy) 2)
      (test-equal 1 (json-get object "a")))))

(define-test test-object-construction-errors
  (test-signals json-error (json-object "a"))
  (test-signals json-error (json-object :a 1))
  (test-assert (not (json-object-p (let ((table (make-hash-table)))
                                     (setf (gethash :a table) 1)
                                     table)))))

(define-test test-booleans
  (dolist (case (list (list t t nil)
                      (list 'yason:true t nil)
                      (list (json-false) nil t)
                      (list 'yason:false nil t)
                      (list nil nil nil)
                      (list ':null nil nil)))
    (destructuring-bind (value true-p false-p) case
      (test-equal true-p (json-true-p value))
      (test-equal false-p (json-false-p value))))
  (test-equal t (json-boolean 42))
  (test-assert (json-false-p (json-boolean nil))))

(define-test test-string-predicates
  (test-assert (json-string= "x" "x"))
  (test-assert (not (json-string= 1 "1")))
  (test-assert (json-string-member-p "b" '("a" "b")))
  (test-assert (not (json-string-member-p :b '("a" "b")))))
