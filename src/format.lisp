(in-package #:argo)

;;;; -- Value Normalization --

(-> json-normalize
    (t &key (:input-false-value t) (:false-value t) (:null-value (member nil :null))
            (:limits (or null json-limits))) t)
(defun json-normalize (value &key (input-false-value (json-false))
                                 (false-value (json-false)) null-value limits)
  "Return an independently owned JSON tree without encoding or parsing it.

Objects become EQUAL tables, arrays simple vectors, true T, false FALSE-VALUE
and null NULL-VALUE. Each occurrence of every container, string and key is
copied separately. Integers retain their value; ratios and floats become
finite double floats, matching the codec's numeric representation.
INPUT-FALSE-VALUE selects the input marker independently of the output marker.
LIMITS apply as in JSON-ENCODE, including compact escaped character count."
  (when limits
    (json--measure value limits input-false-value))
  (let ((copy (json--copy-value value :input-false-value input-false-value
                                    :false-value false-value :null-value null-value
                                    :detached-p t :normalize-numbers-p t)))
    (when limits
      (json--render-value copy :false-value false-value :count-only-p t
                              :maximum-characters (json-limits-maximum-characters limits)))
    copy))


;;;; -- Bounded Formatting --

(-> json--string-encoded-length (string) (integer 2))
(defun json--string-encoded-length (string)
  "Count the exact characters Yason writes for STRING, including both quotes."
  (+ 2 (loop for character across string
             for code = (char-code character)
             sum (cond
                   ((find character '(#\\ #\" #\Backspace #\Page #\Newline #\Return #\Tab))
                    2)
                   ((< code 32)
                    6)
                   ((<= #x10000 code #x10ffff)
                    12)
                   (t
                    1)))))

(-> json--render-value
    (t &key (:false-value t) (:sorted-p boolean) (:indentation (or null (integer 0)))
            (:maximum-characters (or null (integer 0))) (:count-only-p boolean))
    (or string (integer 0)))
(defun json--render-value (value &key (false-value 'yason:false) sorted-p indentation
                                     maximum-characters count-only-p)
  "Render a validated VALUE, checking escaped characters before writing each token.

COUNT-ONLY-P counts tokens without constructing a document. With INDENTATION,
nonempty containers use one item per line; empty containers stay compact."
  (let ((stream (unless count-only-p (make-string-output-stream)))
        (count 0))
    (labels ((reserve (characters)
               (incf count characters)
               (json--check maximum-characters ':characters count))

             (text (string)
               (reserve (length string))
               (when stream (write-string string stream)))

             (line-break (depth)
               (when indentation
                 (reserve (1+ (* depth indentation)))
                 (when stream
                   (terpri stream)
                   (dotimes (index (* depth indentation))
                     (write-char #\Space stream)))))

             (scalar (value)
               (cond
                 ((eq value false-value)
                  (text "false"))
                 ((stringp value)
                  (reserve (json--string-encoded-length value))
                  (when stream (json--yason-encode value stream)))
                 (t
                  (text (with-output-to-string (output)
                          (json--yason-encode value output))))))

             (emit (value depth)
               (cond
                 ((eq value false-value)
                  (scalar value))
                 ((hash-table-p value)
                  (let ((keys (loop for key being the hash-keys of value collect key)))
                    (when sorted-p (setf keys (sort keys #'string<)))
                    (text "{")
                    (loop for key in keys for first-p = t then nil
                          do (unless first-p (text ","))
                             (line-break (1+ depth))
                             (scalar key)
                             (text (if indentation ": " ":"))
                             (emit (gethash key value) (1+ depth)))
                    (when keys (line-break depth))
                    (text "}")))
                 ((and (vectorp value) (not (stringp value)))
                  (text "[")
                  (loop for child across value for first-p = t then nil
                        do (unless first-p (text ","))
                           (line-break (1+ depth))
                           (emit child (1+ depth)))
                  (when (plusp (length value)) (line-break depth))
                  (text "]"))
                 (t
                  (scalar value)))))
      (emit value 0)
      (if stream (get-output-stream-string stream) count))))

(-> json-encode-sorted (t &key (:limits (or null json-limits)) (:false-value t)) string)
(defun json-encode-sorted (value &key limits (false-value (json-false)))
  "Encode normalized VALUE as compact JSON with case-sensitive sorted object keys.

This is an application-specific deterministic profile, not RFC 8785. Number
and marker policies match JSON-DECODE of JSON-ENCODE. LIMITS bound the value
and the actual escaped output, before each token is written."
  (when limits
    (json--measure value limits false-value))
  (let ((prepared (json--copy-value value :input-false-value false-value
                                         :normalize-numbers-p t)))
    (json--render-value prepared :sorted-p t
                                :maximum-characters
                                (and limits (json-limits-maximum-characters limits)))))

(-> json-encode-pretty
    (t &key (:limits (or null json-limits)) (:false-value t) (:indentation (integer 0))) string)
(defun json-encode-pretty (value &key limits (false-value (json-false)) (indentation 2))
  "Encode VALUE with INDENTATION spaces per container level and no final newline.

Preserve object iteration order, scalar spelling and escaped string contents
from JSON-ENCODE. Empty arrays and objects are compact. LIMITS apply to the
input tree and to the actual formatted character count."
  (when limits
    (json--measure value limits false-value))
  (json--render-value (json--encoding-value value false-value)
                      :indentation indentation
                      :maximum-characters
                      (and limits (json-limits-maximum-characters limits))))
