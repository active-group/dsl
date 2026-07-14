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

(struct cell
  (datatype)
  #:transparent)

; record-type is an cell or a list of cell
; sheet is a record or list of records
; all of those we can also call area

(struct record-type
  (table-formats ; list of
   direction)
  #:transparent)

(struct list-type
  (table-format
   direction)
  #:transparent)

(struct header
  (label)
  #:transparent)


; KOMBINATORTYPE !!!! table format is either cell , record-type or header

(define headers1 (record-type ( list (header "Segment") (header "Country") (header "Units Sold") (header "Manuf. Price") (header "Sale Price")) 'horizontal))
(define record-type1 (record-type ( list (cell 'string)(cell 'string)(cell 'number)(cell 'number)(cell 'number)) 'horizontal))
(define entries1 (list-type record-type1 'vertical))


(define format1 (record-type (list header1 entries1) 'vertical))


(define (validate format table-data))
