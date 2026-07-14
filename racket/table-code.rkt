#lang racket

(define table1
  '(("Segment" "Country" "Units Sold" "Manuf. Price" "Sale Price")
    ("Government" "Canada" 1618 3 20)
    ))

(struct entry
  (segment
   country
   units-sold
   manufacturing-price
   sale-price)
  #:transparent)

#;(record entry
        (segment "Segment" string)
        (country "Country" string)
        (units-sold "Units Sold" number)
        (manufacturing-price "Manuf. Price" number)
        (sale-price "Sale Price" number))

(define (table-ref table row column)
  (list-ref (list-ref table row) column))

; Strukturierte Daten: Liste von entry-Structs