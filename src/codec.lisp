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

(-> json--encoding-value (t t) t)
(defun json--encoding-value (value false-value)
  "Return a validated copy of VALUE for Yason, translating FALSE-VALUE.

Containers are copied, nonempty lists become vectors, and circular or
unrepresentable values signal JSON-UNREPRESENTABLE."
  (let ((active (make-hash-table :test #'eq)))
    (labels ((container (container function)
               (when (gethash container active)
                 (json--unrepresentable container "A JSON value contains itself."))
               (setf (gethash container active) t)
               (prog1 (funcall function)
                 (remhash container active)))

             (copy (value)
               (cond
                 ((eq value false-value)
                  'yason:false)
                 ((or (null value) (eq value t) (eq value ':null)
                      (eq value 'yason:true) (eq value 'yason:false))
                  value)
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
                  (container value (lambda () (map 'simple-vector #'copy value))))
                 ((consp value)
                  (unless (json--proper-list-p value)
                    (json--unrepresentable
                     value "A dotted or circular list has no JSON representation."))
                  (container value (lambda () (map 'simple-vector #'copy value))))
                 (t
                  (json--unrepresentable value "~S has no JSON representation." value)))))
      (copy value))))

(-> json--yason-encode (t stream) null)
(defun json--yason-encode (prepared stream)
  "Write PREPARED as compact JSON, independently of ambient boolean bindings."
  (let ((yason:true  'yason:true)
        (yason:false 'yason:false))
    (yason:encode prepared stream))
  nil)

(-> json-encode (t &key (:limits (or null json-limits)) (:false-value t)) string)
(defun json-encode (value &key limits (false-value (json-false)))
  "Encode VALUE as one compact JSON string, translating FALSE-VALUE as false.

Signal JSON-UNREPRESENTABLE when VALUE has no JSON form. With LIMITS, VALUE
is first checked against them and the result must fit their character
bound; JSON-LIMIT-EXCEEDED reports the first excess."
  (when limits
    (json--measure value limits false-value))
  (let* ((prepared (json--encoding-value value false-value))
         (encoded  (with-output-to-string (stream)
                     (json--yason-encode prepared stream))))
    (when limits
      (json--check (json-limits-maximum-characters limits) ':characters
                   (length encoded)))
    encoded))

(-> json-encode-utf8 (t &key (:limits (or null json-limits)) (:false-value t))
    (vector (unsigned-byte 8)))
(defun json-encode-utf8 (value &key limits (false-value (json-false)))
  "Encode VALUE as compact UTF-8 JSON octets.

LIMITS and FALSE-VALUE apply as in JSON-ENCODE. A limited encoding passes
through one string; an unlimited encoding streams octets directly."
  (if limits
      (flexi-streams:string-to-octets
       (json-encode value :limits limits :false-value false-value)
       :external-format ':utf-8)
      (let* ((prepared     (json--encoding-value value false-value))
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

(-> json--parse (string t (member nil :null)) t)
(defun json--parse (source false-value null-value)
  "Build SOURCE's value with explicit markers after JSON--SCAN accepted it."
  (let ((yason:true                    t)
        (yason:false                   false-value)
        (yason:*parse-object-as-alist* nil))
    (handler-case
        (yason:parse source :object-as ':hash-table :object-key-fn #'identity
                            :json-arrays-as-vectors t :json-booleans-as-symbols t
                            :json-nulls-as-keyword (eq null-value ':null))
      (error (condition)
        ;; A valid value may exceed Lisp's numeric range. Parser reports may
        ;; quote the source; retain only the condition type.
        (error 'json-syntax-error
               :position 0
               :message  (format nil "A JSON value has no Lisp representation (~A)."
                                 (type-of condition)))))))

(-> json-decode (string &key (:limits (or null json-limits))
                             (:false-value t) (:null-value (member nil :null))) t)
(defun json-decode (source &key limits (false-value (json-false)) null-value)
  "Decode exactly one RFC 8259 JSON value and surrounding whitespace.

Objects become EQUAL hash tables, arrays vectors, true T, false FALSE-VALUE,
and null NULL-VALUE (NIL or :NULL), independently of ambient Yason bindings.
Malformed input signals payload-free JSON-SYNTAX-ERROR; trailing text signals
its JSON-TRAILING-DATA subclass. LIMITS apply during the grammar scan before
building any values, signaling JSON-LIMIT-EXCEEDED."
  (json--scan source limits)
  (json--parse source false-value null-value))

(-> json-object-source-p (t) boolean)
(defun json-object-source-p (source)
  "Return true when SOURCE is a string holding exactly one JSON object and whitespace."
  (and (stringp source)
       (handler-case
           (json-object-p (json-decode source))
         (json-error ()
           nil))))
