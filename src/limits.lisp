(in-package #:argo)

;;;; -- Limits --

(defstruct (json-limits (:constructor make-json-limits
                            (&key maximum-characters maximum-depth maximum-nodes
                                  maximum-string-characters
                                  maximum-aggregate-string-characters
                                  maximum-object-key-characters
                                  maximum-object-members maximum-array-elements
                                  maximum-number-characters)))
  "Structural bounds for one JSON document; a NIL bound is not enforced.

MAXIMUM-CHARACTERS bounds the source text when decoding and the encoded
output when encoding. The other bounds apply to the value tree on both
sides: nesting depth, total nodes counting object keys, characters in one
string and across all strings, characters in one object key, members in one
object, elements in one array, and characters in one number."
  (maximum-characters nil :type (or null (integer 0)) :read-only t)
  (maximum-depth nil :type (or null (integer 0)) :read-only t)
  (maximum-nodes nil :type (or null (integer 0)) :read-only t)
  (maximum-string-characters nil :type (or null (integer 0)) :read-only t)
  (maximum-aggregate-string-characters nil :type (or null (integer 0)) :read-only t)
  (maximum-object-key-characters nil :type (or null (integer 0)) :read-only t)
  (maximum-object-members nil :type (or null (integer 0)) :read-only t)
  (maximum-array-elements nil :type (or null (integer 0)) :read-only t)
  (maximum-number-characters nil :type (or null (integer 0)) :read-only t))

(defparameter *json-strict-limits*
  (make-json-limits :maximum-characters (* 16 1024 1024)
                    :maximum-depth 64
                    :maximum-nodes 100000
                    :maximum-string-characters (* 12 1024 1024)
                    :maximum-aggregate-string-characters (* 12 1024 1024)
                    :maximum-object-key-characters 4096
                    :maximum-object-members 4096
                    :maximum-array-elements 65536
                    :maximum-number-characters 256)
  "Bounds suited to documents from an untrusted peer.")


;;;; -- Enforcement --

(-> json--exceeded (keyword integer string) nil)
(defun json--exceeded (constraint limit description)
  "Signal that the bound CONSTRAINT, set to LIMIT and described by DESCRIPTION, was exceeded."
  (error 'json-limit-exceeded
         :constraint constraint
         :limit limit
         :message (format nil "The JSON document exceeds its limit of ~:D ~A."
                          limit description)))

(-> json--check (t keyword integer string) null)
(defun json--check (limit constraint value description)
  "Signal when VALUE exceeds LIMIT, unless LIMIT is NIL."
  (when (and limit (> value limit))
    (json--exceeded constraint limit description))
  nil)

(-> json--measure (t json-limits) null)
(defun json--measure (value limits)
  "Check VALUE, as prepared by JSON--ENCODING-VALUE, against LIMITS iteratively.

A prepared value holds only string-keyed hash tables, simple vectors,
strings, numbers, and yason's symbols, and never contains itself."
  (let ((stack (list (cons value 0)))
        (nodes 0)
        (aggregate 0))
    (labels ((node ()
               (incf nodes)
               (json--check (json-limits-maximum-nodes limits) ':nodes nodes "nodes"))

             (text (string key-p)
               (let ((characters (length string)))
                 (json--check (json-limits-maximum-string-characters limits)
                              ':string-characters characters "characters in one string")
                 (when key-p
                   (json--check (json-limits-maximum-object-key-characters limits)
                                ':object-key-characters characters
                                "characters in one object key"))
                 (incf aggregate characters)
                 (json--check (json-limits-maximum-aggregate-string-characters limits)
                              ':aggregate-string-characters aggregate
                              "string characters in total")))

             (enter (depth count constraint limit description)
               (json--check (json-limits-maximum-depth limits) ':depth (1+ depth)
                            "levels of nesting")
               (json--check limit constraint count description)))

      (loop while stack
            for (current . depth) = (pop stack)
            do (node)
               (cond
                 ((hash-table-p current)
                  (enter depth (hash-table-count current) ':object-members
                         (json-limits-maximum-object-members limits)
                         "members in one object")
                  (maphash (lambda (key child)
                             (node)
                             (text key t)
                             (push (cons child (1+ depth)) stack))
                           current))
                 ((stringp current)
                  (text current nil))
                 ((vectorp current)
                  (enter depth (length current) ':array-elements
                         (json-limits-maximum-array-elements limits)
                         "elements in one array")
                  (loop for child across current
                        do (push (cons child (1+ depth)) stack)))
                 ((realp current)
                  (json--check (json-limits-maximum-number-characters limits)
                               ':number-characters
                               (length (with-output-to-string (stream)
                                         (yason:encode current stream)))
                               "characters in one number"))))))
  nil)
