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


(define format1 (record-type (list headers1 entries1) 'vertical))

(define (match-datatype datatype table-data row col)
  (define val (table-ref table-data row col))
  (match datatype
    ('number (if (number? val)
                 val
                 (error 'validate "not a number ~a" val)))
    ('string (if (string? val)
                 val
                 (error 'validate "not a string ~a" val)))
    )
  )

#;
(define (match-record-type r-type table-data row col)
  (match r-type
    ((cell datatype) (match-datatype datatype table-data row col))
    ((header labels) '2)
    ((list-type  record-type direction) '4)
    ((record-type  table-formats direction) '3)
    ))

(define (record-width rec direction)
  (match direction
    ('vertical 0) ;todo later
    ('horizontal 1
                 )
    ))

(define (record-height rec direction)
  (match direction
    ('vertical 1)
    ('horizontal 0) ; todo later
    )
  )

(define (match-record-type table-formats direction table-data row col)
  (match table-formats
    ('() '())
    ((cons first-format rest-formats)
     (cons (validate first-format table-data row col)
           (match-record-type rest-formats direction table-data (+ row (record-height first-format direction)) (+ col (record-width first-format direction)) )))))



(define (match-list-type element-format direction table-data row col)
  (with-handlers ((exn? (lambda (exn) '())))
    (cons
     (validate element-format table-data row col)
     (match-list-type element-format direction table-data (+ row (record-height element-format direction)) (+ col (record-width element-format direction))))))

(define (match-label label table-data row col)
  (define val (table-ref table-data row col))
   (if (equal? val label)
                 val
                 (error 'validate "Wrong label ~a" val))
  )


(define (validate format table-data row col)
  (write (list 'validate format table-data row col)) (newline)
  (match format
    ((cell datatype) (match-datatype datatype table-data row col))
    ((header label) (match-label label table-data row col))
    ((list-type  table-format direction) (match-list-type table-format direction table-data row col))
    ((record-type  table-formats direction)
     (match-record-type table-formats direction table-data row col))
    ))


(module+ test
  (require rackunit)
  (check-equal? (validate format1 table1 0 0)
                  '(("Segment" "Country" "Units Sold" "Manuf. Price" "Sale Price")
  (("Government" "Canada" 1618 3 20))))
  )