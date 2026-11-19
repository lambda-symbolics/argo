(in-package #:argo)

;;;; -- Conditions --

(define-condition json-error (error)
  ((message
    :initarg       :message
    :reader        json-error-message
    :type          string
    :documentation "A human-readable explanation of the failure."))
  (:report (lambda (condition stream)
             (write-string (json-error-message condition) stream)))
  (:documentation "The parent of every condition signalled by argo."))

(define-condition json-syntax-error (json-error)
  ((position
    :initarg       :position
    :reader        json-syntax-error-position
    :type          (integer 0)
    :documentation "The character index where the source stopped being valid JSON."))
  (:documentation
   "The source is not exactly one well-formed JSON value.

The report contains the position and the expected token.
However, it does not contain the source, because it may contain sensitive data."))

(define-condition json-limit-exceeded (json-error)
  ((constraint
    :initarg       :constraint
    :reader        json-limit-exceeded-constraint
    :type          keyword
    :documentation "The exceeded bound, such as :DEPTH or :NODES.")
   (limit
    :initarg       :limit
    :reader        json-limit-exceeded-limit
    :type          integer
    :documentation "The bound's configured value."))
  (:documentation "A document or value exceeded one of its JSON-LIMITS."))

(define-condition json-unrepresentable (json-error)
  ((value
    :initarg       :value
    :reader        json-unrepresentable-value
    :documentation "The value with no JSON representation."))
  (:documentation "Encoding encountered a value that has no JSON representation."))

(define-condition json-trailing-data (json-syntax-error)
  ()
  (:documentation "A complete JSON value is followed by non-whitespace text."))
