#lang racket
; Tabellenformat

; Datentypen in den einzelnen Zellen

; Datensatz pro Zeile / pro ??? gibt's auch in rechteckig

; Header/Leerzeile (oder nicht)

; Liste, da: Richtung

; fehlt: räumliche Anordnung

; Eine Tabellenformat ist eins der folgenden:
; - eine einzelne Zelle -ODER-
; - ein Datensatz bestehend aus mehreren Zellen -ODER-
; - Folge aus beliebig vielen Datensätzen

; Ein Zellenformat ist eins der folgenden:
(struct enum-cell-format
  (values)) ; Liste möglicher konstante Werte


#;(define segment-header-format
  (enum-cell '("Segment")))

(define (header-format header-text)
  (enum-cell-format (cons header-text '())))

(struct integer-cell-format
  ())

(struct string-cell-format
  ())

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


(struct sequence-format
  (relative-position ; von einem Eintrag zum nächsten
   record-format)
  #:transparent)


; DSL-Programm, Entwurf #0:

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
                                   (string-cell-format))
                (record-field-info (relative-position 1 0)
                                   (string-cell-format))
                (record-field-info (relative-position 0 1)
                                   (string-cell-format))
                (record-field-info (relative-position 1 1)
                                   (integer-cell-format))
                (record-field-info (relative-position 0 2)
                                   (integer-cell-format)))))

(define person-sequence-format
  (sequence-format
   (relative-position 2 0)
   person-format))

; Nächstes Programm:

(struct profitability
  (segment ; segment-format
   country ; ...
   units-sold ; integer-format
   manuf-price ; integer-format
   sale-price
   sales
   profit)
  #:transparent)

(define segment-header-format
  (header-format "Segment"))

(define segment-format
  (enum-cell-format '("Government" "Midmarket")))

(define country-format
  (enum-cell-format '("Germany" "Canada" "France" "Mexico")))

(define profitability-format
  (record profitability
          (list (record-field-info (relative-position 0 0)
                                   segment-format)
                (record-field-info (relative-position 1 0)
                                   country-format)
                (record-field-info (relative-position 2 0)
                                   (integer-cell-format))
                (record-field-info (relative-position 3 0)
                                   (integer-cell-format))
                (record-field-info (relative-position 4 0)
                                   (integer-cell-format))
                (record-field-info (relative-position 5 0)
                                   (integer-cell-format))
                (record-field-info (relative-position 6 0)
                                   (integer-cell-format)))))

(define profitabilities-format
  (sequence-format
   (relative-position 0 1)
   profitability-format))

(define profitability-header-format
  (record (lambda (segment country units-sold manuf-price sale-price sales profit)
            'validated)
          (list (record-field-info (relative-position 0 0)
                                   (header-format "Segment"))
                (record-field-info (relative-position 1 0)
                                   (header-format "Country"))
                (record-field-info (relative-position 2 0)
                                   (header-format "Units Sold"))
                (record-field-info (relative-position 3 0)
                                   (header-format "Manuf. Price"))
                (record-field-info (relative-position 4 0)
                                   (header-format "Sale Price"))
                (record-field-info (relative-position 5 0)
                                   (header-format "Sales"))
                (record-field-info (relative-position 6 0)
                                   (header-format "Profit")))))


