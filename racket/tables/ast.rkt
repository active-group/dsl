#lang racket
(provide (struct-out table-definition)
         (struct-out field-definition))
#|
  DEFINE TABLE t3 WITH StructName (STR segment "Segment", STR country "Country")
  |#

(struct table-definition (name
                          struct-name
                          field-definitions
                          )
  #:transparent)

(struct field-definition (type
                          canon-name
                          title)
  #:transparent)

(define t3 (table-definition "t3" "StructName"
                            (list
                             (field-definition 'STR "segment" "Segment")
                             (field-definition 'STR "country" "Country")
                             )))

