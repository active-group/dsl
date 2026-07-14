#lang racket

(struct ident
  (ident)
  #:transparent)

(struct money
  (ident
   number)
  #:transparent)

; (struct datatype
;   ())

(struct cell
  (label
   datatype)
  #:transparent)

(struct reccord
  (cells)
  #:transparent)

(struct sheet
  (labels
   records)
  #:transparent)

