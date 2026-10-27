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
sides. They limit nesting depth, total nodes counting object keys, characters
in one string and across all strings, characters in one object key, members in
one object, elements in one array, and characters in one number."
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
  (make-json-limits :maximum-characters                  (* 16 1024 1024)
                    :maximum-depth                       64
                    :maximum-nodes                       100000
                    :maximum-string-characters           (* 12 1024 1024)
                    :maximum-aggregate-string-characters (* 12 1024 1024)
                    :maximum-object-key-characters       4096
                    :maximum-object-members              4096
                    :maximum-array-elements              65536
                    :maximum-number-characters           256)
  "Bounds suited to documents from an untrusted peer.")


;;;; -- Enforcement --

(defparameter *json-limit-descriptions*
  '((:nodes                       . "nodes")
    (:string-characters           . "characters in one string")
    (:object-key-characters       . "characters in one object key")
    (:aggregate-string-characters . "string characters in total")
    (:depth                       . "levels of nesting")
    (:object-members              . "members in one object")
    (:array-elements              . "elements in one array")
    (:number-characters           . "characters in one number")
    (:characters                  . "characters"))
  "Each limit constraint with the words that describe its bound in error messages.")

(-> json--exceeded (keyword integer) nil)
(defun json--exceeded (constraint limit)
  "Signal that the bound CONSTRAINT, set to LIMIT, was exceeded."
  (error 'json-limit-exceeded
         :constraint constraint
         :limit      limit
         :message    (format nil "The JSON document exceeds its limit of ~:D ~A."
                             limit (rest (assoc constraint *json-limit-descriptions*)))))

(-> json--check (t keyword integer) null)
(defun json--check (limit constraint value)
  "Signal when VALUE exceeds LIMIT for CONSTRAINT, unless LIMIT is NIL."
  (when (and limit (> value limit))
    (json--exceeded constraint limit))
  nil)

(-> json--proper-list-p (list) boolean)
(defun json--proper-list-p (list)
  "Return true when LIST is finite and ends in NIL."
  (let ((slow list)
        (fast list))
    (loop
      (dotimes (step 2)
        (cond
          ((null fast)
           (return-from json--proper-list-p t))
          ((atom fast)
           (return-from json--proper-list-p nil)))
        (setf fast (rest fast)))
      (setf slow (rest slow))
      (when (eq fast slow)
        (return nil)))))

(-> json--measure (t json-limits) null)
(defun json--measure (value limits)
  "Check the caller's VALUE against LIMITS iteratively, before anything recurses into it.

Objects, vectors, and proper non-empty lists are containers. A container
that contains itself, or a dotted or circular list, signals
JSON-UNREPRESENTABLE; other values without a JSON form are left for the
encoder to report."
  (let ((stack     (list (list ':enter value 0)))
        (active    (make-hash-table :test #'eq))
        (nodes     0)
        (aggregate 0))
    (labels ((node ()
               (incf nodes)
               (json--check (json-limits-maximum-nodes limits) ':nodes nodes))

             (text (string key-p)
               (let ((characters (length string)))
                 (json--check (json-limits-maximum-string-characters limits)
                              ':string-characters characters)
                 (when key-p
                   (json--check (json-limits-maximum-object-key-characters limits)
                                ':object-key-characters characters))
                 (incf aggregate characters)
                 (json--check (json-limits-maximum-aggregate-string-characters limits)
                              ':aggregate-string-characters aggregate)))

             (enter (container depth count constraint limit)
               (when (gethash container active)
                 (error 'json-unrepresentable
                        :value   container
                        :message "A JSON value contains itself."))
               (json--check (json-limits-maximum-depth limits) ':depth (1+ depth))
               (json--check limit constraint count)
               (setf (gethash container active) t)
               (push (list ':leave container depth) stack))

             (schedule (child depth)
               (push (list ':enter child (1+ depth)) stack)))

      (loop while stack
            for (kind current depth) = (pop stack)
            do (if (eq kind ':leave)
                   (remhash current active)
                   (progn
                     (node)
                     (cond
                       ((hash-table-p current)
                        (enter current depth (hash-table-count current) ':object-members
                               (json-limits-maximum-object-members limits))
                        (maphash (lambda (key child)
                                   (node)
                                   (when (stringp key)
                                     (text key t))
                                   (schedule child depth))
                                 current))
                       ((stringp current)
                        (text current nil))
                       ((vectorp current)
                        (enter current depth (length current) ':array-elements
                               (json-limits-maximum-array-elements limits))
                        (loop for child across current
                              do (schedule child depth)))
                       ((consp current)
                        (unless (json--proper-list-p current)
                          (error 'json-unrepresentable
                                 :value   current
                                 :message "A dotted or circular list has no JSON representation."))
                        (enter current depth (length current) ':array-elements
                               (json-limits-maximum-array-elements limits))
                        (dolist (child current)
                          (schedule child depth)))
                       ((realp current)
                        (json--check (json-limits-maximum-number-characters limits)
                                     ':number-characters
                                     (length (with-output-to-string (stream)
                                               (yason:encode current stream)))))))))))
  nil)
