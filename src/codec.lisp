(in-package #:argo)

;;;; -- Encoding --

;;; Every value is validated and copied before yason sees it. yason fails on
;;; some values in ways that are not ordinary errors, and the copy also lets
;;; JSON-FALSE markers and lists become what yason expects without touching
;;; the caller's data.

(-> json--unrepresentable (t string &rest t) nil)
(defun json--unrepresentable (value control &rest arguments)
  "Signal JSON-UNREPRESENTABLE for VALUE, described by CONTROL and ARGUMENTS."
  (error 'json-unrepresentable
         :value   value
         :message (apply #'format nil control arguments)))

(-> json--finite-float-p (float) boolean)
(defun json--finite-float-p (float)
  "Return true when FLOAT is neither infinite nor NaN."
  (handler-case
      (progn
        (rational float)
        t)
    (arithmetic-error ()
      nil)))

(-> json--encoding-value (t) t)
(defun json--encoding-value (value)
  "Return a validated copy of VALUE that yason encodes as argo's value model.

False markers become yason's false, non-empty lists become simple vectors,
and objects and arrays are copied, so VALUE itself never changes. Signal
JSON-UNREPRESENTABLE for a value with no JSON form, including a container
that contains itself."
  (let ((active (make-hash-table :test #'eq)))
    (labels ((container (container function)
               (when (gethash container active)
                 (json--unrepresentable container "A JSON value contains itself."))
               (setf (gethash container active) t)
               (prog1 (funcall function)
                 (remhash container active)))

             (copy (value)
               (cond
                 ((or (null value) (eq value t) (eq value ':null)
                      (eq value 'yason:true) (eq value 'yason:false))
                  value)
                 ((eq value ':json-false)
                  'yason:false)
                 ((stringp value)
                  value)
                 ((integerp value)
                  value)
                 ((rationalp value)
                  (coerce value 'double-float))
                 ((floatp value)
                  (unless (json--finite-float-p value)
                    (json--unrepresentable value "JSON numbers are finite, not ~S." value))
                  value)
                 ((hash-table-p value)
                  (container
                   value
                   (lambda ()
                     (let ((copy (make-hash-table :test #'equal
                                                  :size (max 1 (hash-table-count value)))))
                       (maphash (lambda (key child)
                                  (unless (stringp key)
                                    (json--unrepresentable
                                     key "JSON object key ~S is not a string." key))
                                  (setf (gethash key copy) (copy child)))
                                value)
                       copy))))
                 ((vectorp value)
                  (container
                   value
                   (lambda ()
                     (map 'simple-vector #'copy value))))
                 ((consp value)
                  (unless (json--proper-list-p value)
                    (json--unrepresentable
                     value "A dotted or circular list has no JSON representation."))
                  (container
                   value
                   (lambda ()
                     (map 'simple-vector #'copy value))))
                 (t
                  (json--unrepresentable value "~S has no JSON representation." value)))))
      (copy value))))

(-> json--yason-encode (t stream) null)
(defun json--yason-encode (prepared stream)
  "Write the PREPARED value to STREAM as compact JSON."
  (yason:encode prepared stream)
  nil)

(-> json-encode (t &key (:limits (or null json-limits))) string)
(defun json-encode (value &key limits)
  "Encode VALUE as one compact JSON string.

Signal JSON-UNREPRESENTABLE when VALUE has no JSON form. With LIMITS, VALUE
is first checked against them and the result must fit their character
bound; JSON-LIMIT-EXCEEDED reports the first excess."
  (when limits
    (json--measure value limits))
  (let* ((prepared (json--encoding-value value))
         (encoded  (with-output-to-string (stream)
                     (json--yason-encode prepared stream))))
    (when limits
      (json--check (json-limits-maximum-characters limits) ':characters
                   (length encoded) "characters"))
    encoded))

(-> json-encode-utf8 (t &key (:limits (or null json-limits))) (vector (unsigned-byte 8)))
(defun json-encode-utf8 (value &key limits)
  "Encode VALUE as compact UTF-8 JSON octets.

Signal JSON-UNREPRESENTABLE when VALUE has no JSON form. LIMITS apply as in
JSON-ENCODE, whose character bound counts the encoded characters, so a
limited encoding passes through one string; an unlimited one streams the
octets directly."
  (if limits
      (flexi-streams:string-to-octets (json-encode value :limits limits)
                                      :external-format ':utf-8)
      (let* ((prepared     (json--encoding-value value))
             (octet-stream (flexi-streams:make-in-memory-output-stream))
             (character-stream
               (flexi-streams:make-flexi-stream octet-stream :external-format ':utf-8)))
        (unwind-protect
             (progn
               (json--yason-encode prepared character-stream)
               (finish-output character-stream)
               (flexi-streams:get-output-stream-sequence octet-stream))
          (close character-stream)))))


;;;; -- Decoding --

(-> json--parse (string) t)
(defun json--parse (source)
  "Build argo's value for SOURCE, which JSON--SCAN has already accepted."
  (let ((yason:*parse-json-arrays-as-vectors*   t)
        (yason:*parse-json-booleans-as-symbols* t)
        (yason:true                             t)
        (yason:false                            ':json-false))
    (handler-case
        (yason:parse source)
      (error (condition)
        ;; The grammar is already valid, so only a value Lisp cannot hold,
        ;; such as an overflowing exponent, reaches this handler. The
        ;; parser's own report may quote the source, so only its type is kept.
        (error 'json-syntax-error
               :position 0
               :message  (format nil "A JSON value has no Lisp representation (~A)."
                                 (type-of condition)))))))

(-> json-decode (string &key (:limits (or null json-limits))) t)
(defun json-decode (source &key limits)
  "Decode SOURCE, which must hold exactly one RFC 8259 JSON value and whitespace.

Objects become EQUAL hash tables, arrays vectors, true T, false the
JSON-FALSE marker, and null NIL. Malformed input signals JSON-SYNTAX-ERROR,
whose report never quotes SOURCE. With LIMITS, SOURCE is checked during the
grammar scan, before any value is built, signaling JSON-LIMIT-EXCEEDED."
  (json--scan source limits)
  (json--parse source))

(-> json-object-source-p (t) boolean)
(defun json-object-source-p (source)
  "Return true when SOURCE is a string holding exactly one JSON object and whitespace."
  (and (stringp source)
       (handler-case
           (json-object-p (json-decode source))
         (json-error ()
           nil))))
