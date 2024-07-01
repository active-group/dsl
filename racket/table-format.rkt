#lang racket
(provide int-cell)

; Ein Tabellenformat (hat einen Typ) ist ...
; - Cell -ODER-
; - Record -ODER-
; - Sequence

; Cell ist ...
; - eine Int-Cell -ODER-
; - eine Double-Cell -ODER-
; - eine Datum-Cell -ODER-
; - eine Aufzählungs-Cell -ODER- ...
; ...

; Value: "Typ des Formats"

; Funktion: String -> Value -ODER- error
(struct cell
  (validator-function)
  #:transparent)

(struct error
  (description)
  #:transparent)

; Liste von ...
(struct segment-performance
  (segment ; 'government oder 'midmarket
   country ; string
   units-sold ; natural
   manufacturing-price ; double
   sale-price ; double
   sales ; natural
   profit)) ; double

(define int-cell
  (cell (lambda (text)
          (define number (string->number text))
          (cond
            ((not number) (error "not a number"))
            ((not (integer? number))
             (error "not an integer"))
            (else
             number)))))

(define (enum-cell texts)
  (cell (lambda (text)
          (define search (member text texts))
          (cond
            ((not search) (error "not a member"))
            (else (first search))))))

(define (apply-cell cell text)
  ((cell-validator-function cell) text))


; Record

(struct record
  (constructor field-formats)
  #:transparent) ; Liste von Formaten


