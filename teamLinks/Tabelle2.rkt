#lang racket


; tabellenformat ist eins von:
; header cell ODER
; cell ODER
; record ODER
; liste 

(struct header-cell-format
  (name)
  #:transparent)

(struct cell-format
  (type
   )
  #:transparent)

(struct record-format
  (formats ; liste von irgendwelchen values / formaten / namen, whatever
   direction ; 'down oder 'right
   constructor) ; ... vom struct
  #:transparent)

(struct list-format
  (element-format
   direction)
  #:transparent
  )



; --- ^^^^^ DSL


#;(struct entryList ; beschreibt ein row mit beliebig vielen komponenten
  (entries ; list von entries
   );#transparent
   )

(struct prices
  (manufacturing sale)
  #:transparent)

(define price-format
  (record-format (list (cell-format 'number)
                (cell-format 'number))
          'down
          prices))




(define header
  (record-format (list (header-cell-format "Segment")
                (header-cell-format "Country")
                (header-cell-format "Units Sold")
                (header-cell-format "Prices"))
          'right
          list))



(struct entry  ; beschreibt ein row, mit fixen anzahl von komponenten
  (segment country units-sold prices)
  #:transparent)


(define format1
  (record-format (list (cell-format 'string)
                       (cell-format 'string)
                       (cell-format 'number)
                       price-format)
          'right
          entry))



format1


(define table-format
  (record-format (list header
                (list-format format1 'down))
          'down
          (lambda (header-content payload)
            payload)))




(define (table-ref table row column)
  (list-ref (list-ref table row) column))



(define (table-read format table row column)
  (match format
    ((header-cell-format name) 'todo )
    ((cell-format type) 'todo)
    ((record-format formats direction constructor) 'todo)
    ((list-format elemtn-format direction) 'todo)))