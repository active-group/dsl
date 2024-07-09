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
(struct enum-cell
  (row
   column
   values)) ; Liste möglicher konstante Werte

(define segment-format
  (enum-cell 0 0 '("Government" "Midmarket")))

#;(define segment-header-format
  (enum-cell 0 0 '("Segment")))

(define (header-format header-text)
  (enum-cell 0 0 (cons header-text '())))

(define segment-header-format
  (header-format "Segment"))

(struct integer-cell
  (row
   column))

(struct string-cell
  (row
   column))



; Header-Zelle

(struct profitability
  (segment
   country
   units-sold
   manuf-price
   sale-price
   sales
   profit)
  #:transparent)
