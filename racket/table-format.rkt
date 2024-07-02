#lang racket
(provide int-cell)

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
; nimmt den Inhalt des Feldes und liefert entweder einen Wert oder einen error
(struct cell
  (validator-function)
  #:transparent)

(struct error
  (description)
  #:transparent)

; Liste von ...
(struct segment-performance
  (segment ; 'government oder 'midmarket
   country ; string
   units-sold ; natural
   manufacturing-price ; double
   sale-price ; double
   sales ; natural
   profit)) ; double

(define int-cell
  (cell (lambda (text)
          (define number (string->number text))
          (cond
            ((not number) (error "not a number"))
            ((not (integer? number))
             (error "not an integer"))
            (else
             number)))))

(define number-cell
  (cell (lambda (text)
          (define number (string->number text))
          (cond
            ((not number) (error "not a number"))
            (else number)))))

(define (enum-cell texts)
  (cell (lambda (text)
          (define search (member text texts))
          (cond
            ((not search) (error "not a member"))
            (else (first search))))))

(define (apply-cell cell text)
  ((cell-validator-function cell) text))

(module+ test
  (require rackunit)
  (check-equal? (apply-cell int-cell "123") 123)
  (check-equal? (apply-cell int-cell "Mike") (error "not a number"))
  (check-equal? (apply-cell int-cell "123.5") (error "not an integer"))

  (check-equal? (apply-cell (enum-cell (list "Government" "Midmarket"))
                            "Mike")
                (error "not a member"))
  (check-equal? (apply-cell (enum-cell (list "Government" "Midmarket"))
                            "Government")
                "Government"))
  

; Record

(struct record
  (constructor
   direction ; 'right oder 'down
   field-formats)
  #:transparent) ; Liste von Formaten

; Eine Zeile aus der Beispieltabelle

(define segment-performance-format
  (record
   segment-performance
   'right
   (list (enum-cell (list "Government" "Midmarket"))
         (enum-cell (list "Canada" "Germany" "France" "Mexico"))
         int-cell
         number-cell
         number-cell
         number-cell
         number-cell)))

; Liste
(struct sequence
  (direction
   element-format)
  #:transparent)

(define segment-performances-format
  (sequence 'down segment-performance-format))

(define (constant-cell text)
  (enum-cell (list text)))

(define (ignore . args)
  'pupu)

(define title-format
  (record #;(lambda (h1 h2 h3 h4 h5 h6 h7)
            'pupu)
          ignore
   'right
   #;(list (enum-cell (list "Segment"))
         (enum-cell (list "Country"))
         (enum-cell (list "Units Sold"))
         (enum-cell (list "Manuf. Price"))
         ...)
   #;(list (constant-cell "Segment")
         (constant-cell "Country")
         ...)
   (map constant-cell
        (list "Segment"
              "Country"
              "Units Sold"
              "Manuf. Price"
              "Sale Price"
              "Sales"
              "Profit"))))

(define table-format
  (record (lambda (headings segment-performances)
            segment-performances)
   'down
   (list title-format
         segment-performances-format)))
