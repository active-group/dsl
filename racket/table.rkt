#lang racket
; Tabellenformat

; Datentypen in den einzelnen Zellen

; Datensatz pro Zeile / pro ??? gibt's auch in rechteckig

; Header/Leerzeile (oder nicht)

; Liste, da: Richtung

; fehlt: räumliche Anordnung

; Eine Tabellenformat) ist eins der folgenden:
; - eine einzelne Zelle -ODER-
; - ein Datensatz bestehend aus mehreren Zellen -ODER-
; - Folge aus beliebig vielen Datensätzen

; Ein Zellenformat ist eins der folgenden:
(struct enum-cell-format
  (row
   column
   values)) ; Liste möglicher konstante Werte

(define segment-format
  (enum-cell-format 0 0 '("Government" "Midmarket")))

#;(define segment-header-format
  (enum-cell 0 0 '("Segment")))

(define (header-format header-text)
  (enum-cell-format 0 0 (cons header-text '())))

(define segment-header-format
  (header-format "Segment"))

(struct integer-cell-format
  (row
   column))

(struct string-cell-format
  (row
   column))

; Datensatz
; pro Zelle:
; - relative Position -UND-
; - Format

(struct record-field-info
  (relative-position ; zur linken oberen Ecke des Datensatzes
   cell-format)
  #:transparent)

; Eine relative Position:
(struct relative-position
  (right
   down)
  #:transparent)

(struct record
  (constructor
   record-field-infos) ; Liste von record-field-info
  #:transparent)

(struct person
  (first-name
   last-name
   street
   number
   age)
  #:transparent)

(define person-format
  (record person
          (list (record-field-info (relative-position 0 0)
                                   string-cell-format)
                (record-field-info (relative-position 1 0)
                                   string-cell-format)
                (record-field-info (relative-position 0 1)
                                   string-cell-format)
                (record-field-info (relative-position 1 1)
                                   integer-cell-format)
                (record-field-info (relative-position 0 2)
                                   integer-cell-format))))

(struct profitability
  (segment ; segment-format
   country ; ...
   units-sold ; integer-format
   manuf-price ; money-format
   sale-price
   sales
   profit)
  #:transparent)
