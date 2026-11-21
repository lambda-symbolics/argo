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
    (loop for (key value) on key-values by (partial #'nthcdr 2)
          do (unless (stringp key)
               (error 'json-error
                      :message (format nil "JSON object key ~S is not a string." key)))
             (setf (gethash key object) value))
    object))

(-> json-array (&rest t) simple-vector)
(defun json-array (&rest elements)
  "Return a JSON array containing ELEMENTS, which encodes as [] when empty."
  (coerce elements 'simple-vector))

(-> json-object-copy (hash-table) hash-table)
(defun json-object-copy (object)
  "Return a detached shallow copy of JSON OBJECT."
  (let ((copy (make-hash-table :test (hash-table-test object)
                               :size (max 1 (hash-table-count object)))))
    (maphash (lambda (key value)
               (setf (gethash key copy) value))
             object)
    copy))


;;;; -- Access --

(-> json-get-present (hash-table t &key (:false-value t)) (values t boolean))
(defun json-get-present (object key &key (false-value (json-false)))
  "Return KEY from OBJECT and whether the key is present.

Report FALSE-VALUE as NIL without modifying OBJECT. Other values, including
Yason's false symbol unless selected as FALSE-VALUE, are returned unchanged."
  (multiple-value-bind (value present-p) (gethash key object)
    (values (if (eq value false-value) nil value) present-p)))

(-> json-get (hash-table t &optional t) t)
(defun json-get (object key &optional default)
  "Return KEY from OBJECT, or DEFAULT when absent, presenting JSON-FALSE as NIL."
  (multiple-value-bind (value present-p) (json-get-present object key)
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


;;;; -- Structural Access and Equality --

(-> json--object-member (hash-table string) (values t boolean))
(defun json--object-member (object key)
  "Return the case-sensitive string KEY from OBJECT, independently of its hash test."
  (if (eq (hash-table-test object) 'equal)
      (gethash key object)
      (loop for candidate being the hash-keys of object
            when (and (stringp candidate) (string= candidate key))
              do (return (values (gethash candidate object) t))
            finally (return (values nil nil)))))

(-> json--array-p (t) boolean)
(defun json--array-p (value)
  "Return true for a JSON array representation, excluding strings and NIL."
  (or (and (vectorp value) (not (stringp value)))
      (and (consp value) (json--proper-list-p value))))

(-> json-value-equal-p (t t) boolean)
(defun json-value-equal-p (left right)
  "Compare JSON trees by value, without case folding or truth-value coercion.

Object order is irrelevant, arrays are ordered, and numbers compare with =.
The supported true, false and null markers compare within their own kind.
Values must be finite, acyclic JSON trees, as accepted by JSON-ENCODE."
  (labels ((same (left right)
             (cond
               ((or (json-true-p left) (json-true-p right))
                (and (json-true-p left) (json-true-p right)))
               ((or (json-false-p left) (json-false-p right))
                (and (json-false-p left) (json-false-p right)))
               ((or (member left '(nil :null)) (member right '(nil :null)))
                (and (not (null (member left '(nil :null))))
                     (not (null (member right '(nil :null))))))
               ((and (realp left) (realp right))
                (= left right))
               ((or (stringp left) (stringp right))
                (and (stringp left) (stringp right) (string= left right)))
               ((and (json-object-p left) (json-object-p right))
                (and (= (hash-table-count left) (hash-table-count right))
                     (loop for key being the hash-keys of left
                           always (multiple-value-bind (value present-p)
                                      (json--object-member right key)
                                    (and present-p (same (gethash key left) value))))))
               ((and (json--array-p left) (json--array-p right))
                (and (= (length left) (length right))
                     (every #'same left right)))
               (t
                nil))))
    (same left right)))

(-> json--array-index (t integer boolean) (or null (integer 0)))
(defun json--array-index (part length decimal-indices-p)
  "Return PART as an in-bounds array index, optionally accepting decimal strings."
  (cond
    ((and (integerp part) (<= 0 part) (< part length))
     part)
    ((and decimal-indices-p (stringp part) (plusp (length part)))
     (loop with index = 0
           for character across part
           for digit = (and (char<= #\0 character #\9) (digit-char-p character))
           do (unless digit (return nil))
              (setf index (+ (* index 10) digit))
              (when (>= index length) (return nil))
           finally (return index)))
    (t
     nil)))

(-> json-path-get-present (t list &key (:decimal-indices-p boolean)) (values t boolean))
(defun json-path-get-present (value path &key decimal-indices-p)
  "Return the raw value at typed PATH and whether it exists.

Strings select case-sensitive object keys and integers select array positions.
With DECIMAL-INDICES-P, decimal strings also select array positions, only when
traversing an array. False and null retain their markers; an empty PATH is present."
  (dolist (part path (values value t))
    (multiple-value-bind (child present-p)
        (cond
          ((and (hash-table-p value) (stringp part))
           (json--object-member value part))
          ((json--array-p value)
           (let ((index (json--array-index part (length value) decimal-indices-p)))
             (if index
                 (values (elt value index) t)
                 (values nil nil))))
          (t
           (values nil nil)))
      (unless present-p
        (return (values nil nil)))
      (setf value child))))
