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





(define header2
  (record-format (list (header-cell-format "Segment")
                (header-cell-format "Country")
                (header-cell-format "Units Sold")
                (header-cell-format "Manuf. price")
                (header-cell-format "Sale price"))
          'right
          list))

(struct entry2  ; beschreibt ein row, mit fixen anzahl von komponenten
  (segment country units-sold manufacturing-price sales-proce)
  #:transparent)


(define format2
  (record-format (list (cell-format 'string)
                       (cell-format 'string)
                       (cell-format 'number)
                       (cell-format 'number)
                       (cell-format 'number))
          'right
          entry2))



(define table-format2
  (record-format (list header2
                       (list-format format2 'down))
          'down
          (lambda (header-content payload)
            payload)))



(define (table-read format table row column)
  (match format
    ((header-cell-format name)
     (define cell-content (table-ref table row column))
     (if (equal? name cell-content)
         cell-content
         (error 'table-ref "header name does not equal the one defined in format: ~a  /=  ~a" name cell-content)
         ))
    ((cell-format type)
     (define cell-content (table-ref table row column))
     (match type 
       ('string
        (if (string? cell-content)
            cell-content
            (error 'table-ref "cell content is not a string: ~a" cell-content)))
       ('number
        (if (number? cell-content)
            cell-content
            (error 'table-ref "cell content is not a number: ~a" cell-content)))
                     
       ))
    ((record-format formats direction constructor)
     (define (loop formats row column)
       (match formats
         ('() '())
         ((cons format '())
          (cons (table-read format table row column) '()))
         ((cons format rest-formats)
          (cons (table-read format table row column)
                (match direction
                  ('down (loop rest-formats  (+ row (format-height format)) 
                                column))
                  ('right (loop rest-formats row 
                                (+ column (format-width format))))
                )))))
     (apply constructor (loop formats row column)))


    ((list-format element-format direction)
     (define (loop row column)
       (with-handlers ((exn? (lambda (_) '())))
         (cons (table-read element-format table row column)
               (match direction
                 ('right (loop row (+ column (format-width element-format))))
                 ('down (loop (+ row (format-height element-format)) column))))))
     (loop row column)
     )))




(define (format-height format)
  (match format
    ((cell-format type) 1)
    ((header-cell-format name) 1)
    ((record-format formats direction constructor)
     (match direction
       ('right (apply max (map format-height formats)))
       ('down (apply + (map format-height formats)))
       ))
    ((list-format element-format direction)
     (match direction
       ('right (format-height element-format))
       ('down (error 'format-height "invalid format ~a" format))))))

(define (format-width format)
  (match format
    ((cell-format type) 1)
    ((header-cell-format name) 1)
    ((record-format formats direction constructor)
     (match direction
       ('right (apply + (map format-width formats)))
       ('down  (apply max (map format-width formats)))
       ))
    ((list-format element-format direction)
     (match direction
       ('right (error 'format-width "invalid format ~a" format))
       ('down (format-width element-format))))))

(define table1
  '(("Segment" "Country" "Units Sold" "Manuf. price" "Sale price")
    ("Government" "Canada" 1618 3 20)
    ))

table1
(table-read (cell-format 'string) '(("Mike")) 0 0)
(table-read (header-cell-format "Country") '(("Country")) 0 0)
(table-read header2 table1 0 0)



(define table3
  '(("Segment" "Country" "Units Sold" "Manuf. price" "Sale price")
    ("Government" "Canada" 1618 3 20)
    ("Government" "Germany" 1321 3 20)
    ("Midmarket" "France" 2178 3 15)
    ("Midmarket" "Germany" 888 3 15)
    ("Midmarket" "Mexico" 2470 	3 15)))

(table-read table-format2 table3 0 0)
