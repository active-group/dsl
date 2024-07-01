#lang racket
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
  (validator-function))

(struct error
  (description))

(define int-cell
  (cell (lambda (text)
          (define number (string->number text))
          (cond
            ((not number) (error "not a number"))
            ((not (integer? number))
             (error "not an integer"))
            (else
             number)))))
            
