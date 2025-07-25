#lang racket

#|
  DEFINE TABLE t3 WITH StructName (STR segment "Segment", STR country "Country")
  |#

(struct table-definition (name
                          struct-name
                          field-definitions
                          ))

(struct field-definition (type
                          canon-name
                          title))

(define t3 (table-definition "t3" "StructName"
                            (list
                             (field-definition 'STR "segment" "Segment")
                             (field-definition 'STR "country" "Country")
                             )))

