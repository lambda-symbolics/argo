(in-package #:argo)

;;;; -- Grammar Scanner --

;;; JSON-DECODE scans every document against the RFC 8259 grammar before any
;;; value is built. The scan is iterative, so deep nesting cannot exhaust the
;;; stack, it enforces JSON-LIMITS as it goes, and its errors name a position
;;; and the expected token without quoting the source, which may hold secrets.

(-> json--whitespace-p (character) boolean)
(defun json--whitespace-p (character)
  "Return true when CHARACTER is JSON whitespace."
  (not (null (find character '(#\Space #\Tab #\Newline #\Return)))))

(-> json--syntax (integer string &rest t) nil)
(defun json--syntax (position control &rest arguments)
  "Signal JSON-SYNTAX-ERROR at POSITION, describing it with CONTROL and ARGUMENTS."
  (error 'json-syntax-error
         :position position
         :message  (format nil "~? at character ~D."
                           control arguments position)))

(-> json--scan (string (or null json-limits)) null)
(defun json--scan (source limits)
  "Check that SOURCE holds exactly one JSON value and whitespace, within LIMITS.

Signal JSON-SYNTAX-ERROR for the first grammar violation and
JSON-LIMIT-EXCEEDED for the first exceeded bound. A NIL LIMITS checks only
the grammar."
  (let ((length    (length source))
        (index     0)
        ;; Each open container is a cons of :OBJECT or :ARRAY and its count.
        (stack     '())
        (state     ':value)
        (nodes     0)
        (aggregate 0))
    (labels ((bound (accessor)
               (and limits (funcall accessor limits)))

             (node ()
               (incf nodes)
               (json--check (bound #'json-limits-maximum-nodes) ':nodes nodes))

             (complete ()
               (setf state (if stack ':next ':end)))

             (open-container (kind)
               (node)
               (push (cons kind 0) stack)
               (json--check (bound #'json-limits-maximum-depth) ':depth (length stack))
               (incf index)
               (setf state (if (eq kind ':object) ':key-or-close ':value-or-close)))

             (close-container ()
               (pop stack)
               (incf index)
               (complete))

             (count-member ()
               (let ((container (first stack)))
                 (incf (rest container))
                 (if (eq (first container) ':object)
                     (json--check (bound #'json-limits-maximum-object-members)
                                  ':object-members (rest container))
                     (json--check (bound #'json-limits-maximum-array-elements)
                                  ':array-elements (rest container)))))

             (scan-string (key-p)
               (node)
               (incf index)
               (let ((characters 0))
                 (loop
                   (when (>= index length)
                     (json--syntax index "Unterminated string"))
                   (let ((character (char source index)))
                     (cond
                       ((char= character #\")
                        (incf index)
                        (return))
                       ((char= character #\\)
                        (scan-escape))
                       ((< (char-code character) #x20)
                        (json--syntax index "Unescaped control character in a string"))
                       (t
                        (incf index))))
                   (incf characters)
                   (incf aggregate)
                   (json--check (bound #'json-limits-maximum-string-characters)
                                ':string-characters characters)
                   (when key-p
                     (json--check (bound #'json-limits-maximum-object-key-characters)
                                  ':object-key-characters characters))
                   (json--check (bound #'json-limits-maximum-aggregate-string-characters)
                                ':aggregate-string-characters aggregate))))

             (scan-escape ()
               (let ((escape (and (< (1+ index) length) (char source (1+ index)))))
                 (cond
                   ((null escape)
                    (json--syntax index "Unterminated string"))
                   ((find escape "\"\\/bfnrt")
                    (incf index 2))
                   ((char= escape #\u)
                    (loop for offset from 2 below 6
                          for position = (+ index offset)
                          unless (and (< position length)
                                      (digit-char-p (char source position) 16))
                            do (json--syntax position "Expected a hexadecimal digit"))
                    (incf index 6))
                   (t
                    (json--syntax (1+ index) "Invalid escape character")))))

             (scan-digits ()
               (let ((start index))
                 (loop while (and (< index length) (digit-char-p (char source index)))
                       do (incf index))
                 (when (= start index)
                   (json--syntax index "Expected a digit"))))

             (scan-number ()
               (node)
               (let ((start index))
                 (when (char= (char source index) #\-)
                   (incf index))
                 (cond
                   ((and (< index length) (char= (char source index) #\0))
                    (incf index))
                   (t
                    (scan-digits)))
                 (when (and (< index length) (char= (char source index) #\.))
                   (incf index)
                   (scan-digits))
                 (when (and (< index length) (char-equal (char source index) #\e))
                   (incf index)
                   (when (and (< index length) (find (char source index) "+-"))
                     (incf index))
                   (scan-digits))
                 (json--check (bound #'json-limits-maximum-number-characters)
                              ':number-characters (- index start))))

             (scan-literal (literal)
               (node)
               (let ((end (+ index (length literal))))
                 (unless (and (<= end length)
                              (string= literal source :start2 index :end2 end))
                   (json--syntax index "Expected ~A" literal))
                 (setf index end)))

             (scan-value (character)
               (when stack
                 (count-member))
               (case character
                 (#\{
                  (open-container ':object))
                 (#\[
                  (open-container ':array))
                 (#\"
                  (scan-string nil)
                  (complete))
                 ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                  (scan-number)
                  (complete))
                 (#\t
                  (scan-literal "true")
                  (complete))
                 (#\f
                  (scan-literal "false")
                  (complete))
                 (#\n
                  (scan-literal "null")
                  (complete))
                 (t
                  (json--syntax index "Expected a JSON value"))))

             (scan-key (character)
               (unless (char= character #\")
                 (json--syntax index "Expected a string object key"))
               (count-member)
               (scan-string t)
               (setf state ':colon)))

      (json--check (bound #'json-limits-maximum-characters) ':characters length)
      (loop
        (loop while (and (< index length) (json--whitespace-p (char source index)))
              do (incf index))
        (when (>= index length)
          (if (eq state ':end)
              (return)
              (json--syntax index "Unexpected end of input")))
        (let ((character (char source index)))
          (ecase state
            (:end
             (json--syntax index "Unexpected text after the JSON value"))
            (:value
             (scan-value character))
            (:value-or-close
             (if (char= character #\])
                 (close-container)
                 (scan-value character)))
            (:key-or-close
             (if (char= character #\})
                 (close-container)
                 (scan-key character)))
            (:key
             (scan-key character))
            (:colon
             (unless (char= character #\:)
               (json--syntax index "Expected ':' after an object key"))
             (incf index)
             (setf state ':value))
            (:next
             (let* ((object-p (eq (first (first stack)) ':object))
                    (closer   (if object-p #\} #\])))
               (cond
                 ((char= character #\,)
                  (incf index)
                  (setf state (if object-p ':key ':value)))
                 ((char= character closer)
                  (close-container))
                 (t
                  (json--syntax index "Expected ',' or '~C'" closer)))))))))
    nil))
