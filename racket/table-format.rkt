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
   profit) ; double
  #:transparent)

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
   field-formats) ; Liste von Formaten
  #:transparent) 

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

(define (heading direction . heading-texts)
  (record ignore
          direction
          (map constant-cell heading-texts)))

(define right 'right)

(define table-format
  (record (lambda (headings segment-performances)
            segment-performances)
          'down
          (list (heading right "Segment" "Country" "Units Sold" "Manuf. Price" "Sale Price" "Sales" "Profit")
                segment-performances-format)))


; 1. Noch so'n Tabellen-Beispiel
; 2. Format für das Tabellen-Beispiel
; 3. Abstraktion über den beiden Tabellen-Beispielen
; 4. die auch benutzen

(define (safe-add o1 o2) ; o1, o2 können Zahlen sein oder error
  (cond
    ((error? o1) o1)
    (else
     (cond
       ((error? o2) o2)
       (else (+ o1 o2))))))


; class Optional<T> {
;   <U> Optional<U> flatMap(Function<T, Optional<U>> mapper)
; }

; optional von T: T oder error

(define (flatmap optional next)
  (cond
    ((error? optional) optional)
    (else
     (next optional))))

(define (safe-add* o1 o2)
  (flatmap o1
           (lambda (n1)
             (flatmap o2
                      (lambda (n2)
                        (+ n1 n2))))))

(define (safe-multiply* o1 o2)
  (flatmap o1
           (lambda (n1)
             (flatmap o2
                      (lambda (n2)
                        (* n1 n2))))))

; Pattern: (flatmap o (lambda (x) ...))

; über Syntax abstrahieren

(define-syntax-rule
  (flatlet name optional body)
  (flatmap optional
           (lambda (name)
             body)))

(define (safe-add** o1 o2)
  (flatlet n1
           o1
           (flatlet n2
                    o2
                    (+ n1 n2))))

#;(define-syntax-rule
  (flatlet+ ((name optional) ...) body)
  ...)

; Plan: (flatlet+ ((name1 optional1) (name-rest optional-rest) ...) body)
;      -> (flatlet name1 optional1 (flatlet+ ((name-rest optional-rest) ...) body)
;       (flatlet+ () body)
;      -> body

(define-syntax flatlet+
  (syntax-rules ()
    ; ähnlich wie match
    ; (<Pattern> <Ersatz>)
    ((flatlet+ ((name1 optional1) (name-rest optional-rest) ...) body)
     (flatlet name1 optional1 (flatlet+ ((name-rest optional-rest) ...) body)))
    ((flatlet+ () body)
     body)))
    
(define (safe-add*** o1 o2)
  (flatlet+ ((n1 o1)
             (n2 o2))
            (+ n1 n2)))

(define (safe-add**** . os)
  (match os
    ('() 0)
    ((cons first rest)
     (flatlet+ ((n first)
                (n-rest (apply safe-add**** rest)))
               (+ n n-rest)))))
               
(define (optionals->list optionals)
  (match optionals
    ('() '())
    ((cons first rest)
     (flatlet+ ((val first)
                (vals (optionals->list rest)))
               (cons val vals)))))
               

(define (optional-map f . os)
  (flatlet+ ((args (optionals->list os)))
            (apply f args)))


; gegeben Format und Tabelle und Position -> Wert oder error

; Tabelle repräsentiert durch Funktion (x y -> string oder error)

(struct extent
  (width height)
  #:transparent)

(define unit-extent (extent 1 1))

(define (mapm f list)
  (match list
    ('() '())
    ((cons first rest)
     (flatlet+ ((result (f first))
                (rest-results (mapm f rest)))
               (cons result rest-results)))))

(define (format-extent format)
  (match format
    ((cell _) unit-extent)
    ((record constructor direction field-formats)
     (define extents (map format-extent field-formats))
     (define widths (map extent-width extents))
     (define heights (map extent-height extents))
     (match direction
       ('right (extent (apply + widths)
                       (apply max heights)))
       ('down (extent (apply max widths)
                      (apply + heights)))))
    
    ((sequence direction element-format)
     (define element-extent (format-extent element-format))
     (match direction
       ('right (extent +inf.0 (extent-height element-extent)))
       ('down (extent (extent-width element-extent) +inf.0))))))

(module+ test
  (check-equal? (format-extent int-cell)
                (extent 1 1))
  (check-equal? (format-extent table-format)
                (extent 7 +inf.0)))

(define (parse-field-tables direction field-formats table x y)
  (match field-formats
    ('() '())
    ((cons first rest)
     (define first-extent (format-extent first))
     (flatlet+ ((first-content (parse-table first table x y))
                (rest-contents
                 (match direction
                   ('right                 
                    (parse-field-tables direction
                                        rest
                                        table
                                        (+ x (extent-width first-extent))
                                        y))
                   ('down
                    (parse-field-tables direction
                                        rest
                                        table
                                        x
                                        (+ y (extent-height first-extent)))))))
               (cons first-content rest-contents)))))

(define (parse-sequence-tables direction element-format table x y)
  (match (table x y)
    ((error _) '())
    (content
     (define element-extent (format-extent element-format))
     (cons content
           (match direction
             ('right
              (parse-table element-format table
                           (+ x (extent-width format-extent))
                           y))
             ('down
              (parse-table element-format table
                           x
                           (+ y (extent-height format-extent)))))))))
      
(define (parse-table format table x y)
  (match format
    ((cell validator-function)
     (validator-function (table x y)))
    ((record constructor direction field-formats)
     (flatlet+ ((field-contents
                 (parse-field-tables direction field-formats table x y)))
               (apply constructor field-contents)))
    ((sequence direction field-format)
     'todo)))

  
; Liste von Zeilen
(define (llist->table llist)
  (lambda (x y)
    (cond
      ((>= y (length llist))
       (error "not enough rows"))
      (else
       (let ((row (list-ref llist y)))
         (cond
           ((>= x (length row))
            (error "not enough columns"))
           (else
            (list-ref row x))))))))

(define performance-table
  (llist->table
   '(("Segment" "Country" "Units Sold" "Manuf. Price" "Sale Price" "Sales" "Profit")
     ("Government" "Canada" "1618"   "3.00"  "20.00" "32370.00" "16185.00")
     ("Government" "Germany" "1321" "3.00" "20.00" "26420.00" "13210.00")
     ("Midmarket"  "France" "2178" "3.00"  "15.00" "32670.00" "10890.00")
     ("Midmarket"  "Germany" "888"  "3.00" "15.00" "13320.00" "4440.00")
     ("Midmarket" "Mexico" "2470" "3.00" "15.00" "37050.00" "12350.00"))))
