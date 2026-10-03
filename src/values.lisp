(in-package #:argo)

;;;; -- The Value Model --

;;; JSON values map to Lisp values as follows.
;;;
;;;   object  an EQUAL hash table with string keys
;;;   array   a vector; decoding produces vectors, and a proper non-empty
;;;           list also encodes as an array
;;;   string  a string
;;;   number  an integer or a finite float; decoded fractions are double
;;;           floats, and a ratio encodes as one
;;;   true    T
;;;   false   the marker returned by JSON-FALSE
;;;   null    NIL; :NULL also encodes as null
;;;
;;; Decoded false is a distinct marker so that a document re-encodes exactly,
;;; and JSON-GET reports it as NIL so that false and null both read as Lisp
;;; false. Write false with JSON-FALSE or JSON-BOOLEAN; a NIL value encodes as
;;; null, and so does an empty list, so build arrays that may be empty with
;;; JSON-ARRAY.

(-> json-object-p (t) boolean)
(defun json-object-p (value)
  "Return true when VALUE is a string-keyed hash table suitable for JSON."
  (and (hash-table-p value)
       (loop for key being the hash-keys of value
             always (stringp key))))

(deftype json-object ()
  "A string-keyed hash table representing a JSON object."
  '(satisfies json-object-p))

(deftype json-value ()
  "A value argo can encode, before its contents are checked."
  '(or null string number symbol vector list json-object))


;;;; -- Booleans --

(-> json-false () keyword)
(defun json-false ()
  "Return the marker that represents JSON false."
  ':json-false)

(-> json-false-p (t) boolean)
(defun json-false-p (value)
  "Return true when VALUE represents JSON false."
  (or (eq value ':json-false)
      (eq value 'yason:false)))

(-> json-true-p (t) boolean)
(defun json-true-p (value)
  "Return true when VALUE represents JSON true."
  (or (eq value t)
      (eq value 'yason:true)))

(-> json-boolean (t) (or (eql t) keyword))
(defun json-boolean (generalized-boolean)
  "Return T or the JSON false marker for GENERALIZED-BOOLEAN."
  (if generalized-boolean t (json-false)))


;;;; -- Construction --

(-> json-object (&rest t) json-object)
(defun json-object (&rest key-values)
  "Return a string-keyed JSON object built from alternating KEY-VALUES."
  (unless (evenp (length key-values))
    (error 'json-error
           :message "JSON objects require an even number of key and value arguments."))
  (let ((object (make-hash-table :test #'equal)))
    (loop for (key value) on key-values by #'cddr
          do (unless (stringp key)
               (error 'json-error
                      :message (format nil "JSON object key ~S is not a string." key)))
             (setf (gethash key object) value))
    object))

(-> json-array (&rest t) simple-vector)
(defun json-array (&rest elements)
  "Return a JSON array containing ELEMENTS, which encodes as [] when empty."
  (coerce elements 'simple-vector))

(-> json-object-copy (json-object) json-object)
(defun json-object-copy (object)
  "Return a detached shallow copy of JSON OBJECT."
  (let ((copy (make-hash-table :test (hash-table-test object)
                               :size (max 1 (hash-table-count object)))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             object)
    copy))


;;;; -- Access --

(-> json-get-present (json-object string) (values t boolean))
(defun json-get-present (object key)
  "Return KEY from JSON OBJECT and whether the key is present.

The JSON-FALSE marker is reported as NIL, while it stays in OBJECT so that
re-encoding preserves the distinction from JSON null. Any other value,
including yason's false symbol, is returned exactly as stored."
  (multiple-value-bind (value present-p)
      (gethash key object)
    (values (if (eq value (json-false)) nil value)
            present-p)))

(-> json-get (json-object string &optional t) t)
(defun json-get (object key &optional default)
  "Return KEY from JSON OBJECT, or DEFAULT when the key is absent.

The JSON-FALSE marker is reported as NIL, as by JSON-GET-PRESENT."
  (multiple-value-bind (value present-p)
      (json-get-present object key)
    (if present-p value default)))

(-> json-string= (t string) boolean)
(defun json-string= (value expected)
  "Return true when VALUE is the JSON string EXPECTED."
  (and (stringp value) (string= value expected)))

(-> json-string-member-p (t list) boolean)
(defun json-string-member-p (value expected)
  "Return true when VALUE is a JSON string in EXPECTED."
  (not (null (and (stringp value)
                  (member value expected :test #'string=)))))
