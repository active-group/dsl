#lang racket

; Tabellenformat

; Datentypen in einzelnen Zellen
; Datensatz pro Zeile / pro ??? gibts auch in rechteckig
; Header/Leerzeile oder auch nicht

; Komposition von Matrix-Tabellen geht nicht so gut

; Liste, da: Richtung

; fehlt raeumliche Anordnung
   
; einfachste Entitaet Ein-Elementige Tabelle (Leere Tabelle spaeter)
; Tabelle(nformat) ist eins der folgenden: --> Gruppe 1 also Typen nicht konkrete Werte (FORMAT nicht Tabelle)
; - eine einzelne Zelle -ODER-
; - ein Datensatz (feste Anzahl an Zellen) -ODER-
; - Folge aus mehreren Datensaetzen (beliebig viele)

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
   format)
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
   format)
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

(define profitabilities-sheet-format
  (record (lambda (header profitabilities)
            profitabilities)
          (list (record-field-info (relative-position 0 0)
                                   profitability-header-format)
                (record-field-info (relative-position 0 1)
                                   profitabilities-format))))
(define (llist->table llist)
  (lambda (x y)
    (list-ref (list-ref llist y) x)))

(define profitabilities-table
  (llist->table
   '(("Segment" "Country" "Units Sold" "Manuf. Price" "Sale Price" "Sales" "Profit")
     ("Government" "Canada" "1618"   "3.00"  "20.00" "32370.00" "16185.00")
     ("Government" "Germany" "1321" "3.00" "20.00" "26420.00" "13210.00")
     ("Midmarket"  "France" "2178" "3.00"  "15.00" "32670.00" "10890.00")
     ("Midmarket"  "Germany" "888"  "3.00" "15.00" "13320.00" "4440.00")
     ("Midmarket" "Mexico" "2470" "3.00" "15.00" "37050.00" "12350.00"))))


(define (table-at table x0 y0)
  (lambda (x y)
    (table (+ x x0)
           (+ y y0))))

; Rueckgabewert: Daten
(define (parse-table format table)
  (match format
    ((enum-cell-format values) (validate-enum-cell (table 0 0) values))
    ((integer-cell-format) (validate-integer-cell (table 0 0)))
    ((string-cell-format) (table 0 0))
    ;((record-field-info (relative-position x y) format) (table-at ))
    ((record constructor record-field-infos) (apply constructor
                                                    (list-from-record-field-infos record-field-infos table)))
    ((sequence-format (relative-position x y) format) (parse-sequence-format x y format table))))

(define (validate-enum-cell value values)
  (cond
    ((member value values) value)
    (else (error "not an enum: " value values))))

(define (validate-integer-cell value)
  (define int-value (string->number value))
  (cond
    ((integer? int-value) int-value)
    (else (error "not an int: " value))))
  

(define (parse-sequence-format x y format table)
  (with-handlers ((exn:fail? (lambda (exn) (list))))
    (cons (parse-table format table)
          (parse-sequence-format x y format (table-at table x y)))))

(define (list-from-record-field-infos record-field-infos table)
  (map (lambda (rfi)
         (match rfi
           ((record-field-info (relative-position x y) format) (parse-table format (table-at table x y)))))
       record-field-infos))
  
;(parse-table (enum-cell-format (list "bla" "blub")) profitabilities-table)
;(parse-table profitability-format profitabilities-table) ; funzt nur weil validierung nicht funzt :)
(parse-table profitabilities-sheet-format profitabilities-table)