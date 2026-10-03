(defpackage #:argo
  (:use #:cl)
  (:import-from #:serapeum
                #:->)
  (:export
   ;; Values
   #:json-object
   #:json-object-p
   #:json-value
   #:json-array
   #:json-object-copy
   #:json-get
   #:json-get-present
   #:json-false
   #:json-false-p
   #:json-true-p
   #:json-boolean
   #:json-string=
   #:json-string-member-p
   ;; Codec
   #:json-encode
   #:json-encode-utf8
   #:json-decode
   #:json-object-source-p
   ;; Limits
   #:json-limits
   #:make-json-limits
   #:json-limits-maximum-characters
   #:json-limits-maximum-depth
   #:json-limits-maximum-nodes
   #:json-limits-maximum-string-characters
   #:json-limits-maximum-aggregate-string-characters
   #:json-limits-maximum-object-key-characters
   #:json-limits-maximum-object-members
   #:json-limits-maximum-array-elements
   #:json-limits-maximum-number-characters
   #:*json-strict-limits*
   ;; Conditions
   #:json-error
   #:json-error-message
   #:json-syntax-error
   #:json-syntax-error-position
   #:json-limit-exceeded
   #:json-limit-exceeded-constraint
   #:json-limit-exceeded-limit
   #:json-unrepresentable
   #:json-unrepresentable-value))
