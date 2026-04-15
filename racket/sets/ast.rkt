#lang racket/base
(require racket/contract)

(define srcloc/c
  (or/c #f
        srcloc?))

(struct node (srcloc) #:transparent)

; set-assignment -> IDENTIFIER ASSIGN set-definition
(struct set-assignment node
  (set-identifier
   set-definition)
  #:transparent)

; set-definition -> LEFT_BRACE set-definition-body RIGHT_BRACE
(struct set-definition node
  (set-definition-body)
  #:transparent)

; set-definition-body -> list-of-numbers
;                     |  element-definition WITH list-set-constraints
(struct constrained-set-definition node
  (element-definition
   list-set-constraints)
  #:transparent)

; element-definition -> IDENTIFIER IN INDENTIFIER
(struct element-definition node
  (element-identifier
   set-identifier)
  #:transparent)

; list-set-constraints -> set-constraint
;                      |  set-constraint COMMA list-set-constraints
(struct set-constraint-with-list-set-constraints node
  (constraint
   list-constraints)
  #:transparent)

; set-constraint -> table-lookup EQUAL value
(struct set-constraint node
  (lookup
   value)
  #:transparent)

; table-lookup -> IDENTIFIER LEFT-PAREN IDENTIFIER RIGHT-PAREN
(struct table-lookup node
  (table-identifier
   index-identifier)
  #:transparent)

; value -> NUMBER | DPPLUS | DPMINUS

(provide
 (struct-out node)
 (struct-out set-assignment)
 (struct-out set-definition)
 (struct-out constrained-set-definition)
 (struct-out element-definition)
 (struct-out set-constraint-with-list-set-constraints)
 (struct-out set-constraint)
 (struct-out table-lookup))