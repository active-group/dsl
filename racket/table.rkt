#lang racket
; Tabellenformat

; Datentypen in den einzelnen Zellen

; Datensatz pro Zeile / pro ??? gibt's auch in rechteckig

; Header/Leerzeile (oder nicht)

; Liste, da: Richtung

; fehlt: räumliche Anordnung

; Eine Tabellenformat ist eins der folgenden:
; - eine einzelne Zelle -ODER-
; - ein Datensatz bestehend aus mehreren Tabellenformaten -ODER-
; - Folge aus beliebig vielen Tabellenformaten

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

; Tabelle: Funktion (x-coordinate y-coordinate -> string)

(struct coordinates-out-of-bounds
  (table x y)
  #:transparent)

(define (llist->table llist)
  (lambda (x y)
    (list-ref (list-ref llist y) x)))

(define (table-does-not-fail table)
  (lambda (x y)
    (with-handlers ; wie try
        ((exn:fail?
          (lambda (exn)
            (coordinates-out-of-bounds table x y))))
      (table x y))))

#;(define (llist->table llist)
  (lambda (x y)
    (cond
      ((and (<= 0 y)
            (< y (length lllist)))
       (define row (list-ref llist y))
       (cond
         ((and (<= 0 x)
               (< x (length row)))
          (list-ref row x))
         (else
          (coordinates-of-bounds llist x y))))
      (else
       (coordinates-of-bounds llist x y)))))

; Tabelle, bei der das, was bei table bei (x, y) liegt,
; bei (0, 0) liegt
(define (table-at table x0 y0)
  (lambda (x y)
    (table (+ x x0)
           (+ y y0))))

; Rückgabewert: Daten der Tabelle
(define (parse-table format table)
  (match format
    ((enum-cell-format values)
     (define content (table 0 0))
     (define rest (member content values))
     (cond
       (rest content)
       (else (error "not in enum" content values))))
    ((string-cell-format) (table 0 0))
    ((integer-cell-format)
     (define content (table 0 0))
     (define parsed (string->number content))
     (cond
       ((integer? parsed) parsed)
       (else (error "not an integer" content))))
    ((record constructor record-field-infos)
     (apply constructor
            (map (lambda (record-field-info)
                   (parse-record-field record-field-info table))
                 record-field-infos)))
    ((sequence-format relative-position format)
     (parse-sequence-format relative-position format table))))

(define (parse-sequence-format relative-position format table)
  (with-handlers
      ((exn:fail?
        (lambda (exn)
          '())))
    (cons (parse-table format table)
          (parse-sequence-format
           relative-position format
           (table-at table
                     (relative-position-right relative-position)
                     (relative-position-down relative-position))))))

(define (parse-record-field rfi table)
    (match rfi
      ((record-field-info (relative-position right down) format)
       (parse-table format (table-at table right down)))))
    

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

#;(struct profitability
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

#;(define profitability-format
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



(define (record* constructor direction . field-formats)
  (record constructor
          (generate-record-field-infos direction field-formats
                                       (relative-position 0 0))))

(define (generate-record-field-infos direction formats relative-position)
  (match formats
    ('() '())
    ((cons format rest-formats)
     (cons
      (record-field-info relative-position format)
      (generate-record-field-infos
       direction rest-formats
       (move-relative-position relative-position direction))))))

(define (move-relative-position relpos direction)
  (match direction
    ('right (relative-position (+ (relative-position-right relpos)
                                  1)
                               (relative-position-down relpos)))
    ('down (relative-position (relative-position-right relpos)
                              (+ (relative-position-down relpos)
                                 1)))))

; kann man eigentlich nur zusammen mit dem struct verstehen
#;(define profitability-format
  (record* profitability
           'right
           segment-format
           country-format
           (integer-cell-format)
           (integer-cell-format)
           (integer-cell-format)
           (integer-cell-format)
           (integer-cell-format)))

#;(define profitabilities-format
  (sequence-format
   (relative-position 0 1)
   profitability-format))

#;(define profitability-header-format
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

#;(define profitabilities-sheet-format
  (record (lambda (header profitabilities)
            profitabilities)
          (list (record-field-info (relative-position 0 0)
                                   profitability-header-format)
                (record-field-info (relative-position 0 1)
                                   profitabilities-format))))


(define profitabilities-table
  (llist->table
   '(("Segment" "Country" "Units Sold" "Manuf. Price" "Sale Price" "Sales" "Profit")
     ("Government" "Canada" "1618"   "3.00"  "20.00" "32370.00" "16185.00")
     ("Government" "Germany" "1321" "3.00" "20.00" "26420.00" "13210.00")
     ("Midmarket"  "France" "2178" "3.00"  "15.00" "32670.00" "10890.00")
     ("Midmarket"  "Germany" "888"  "3.00" "15.00" "13320.00" "4440.00")
     ("Midmarket" "Mexico" "2470" "3.00" "15.00" "37050.00" "12350.00"))))

#;(define (default-data-set format-name struct-name . columns)
  'todo)

; Makro
(define-syntax default-data-set
  (syntax-rules (column) ; wenn im Pattern column, muß dort genau das Wort stehen
    ((default-data-set format-name struct-name
       (column column-format field-name heading-name)
       ...)
     (begin
       (struct struct-name
         (field-name ...)
         #:transparent)
       (define format-name
         (record (lambda (heading record-list)
                   record-list)
                 (list (record-field-info
                        (relative-position 0 0)
                        (record*
                         (lambda headers 'ignored-header)
                         'right
                         (header-format heading-name) ...))

                       (record-field-info
                        (relative-position 0 1)
                        (sequence-format
                         (relative-position 0 1)
                         (record*
                          struct-name
                          'right
                          column-format ...))))))
       
     ))))

(default-data-set profitabilities-sheet-format
  profitability
  (column segment-format segment "Segment")
  (column country-format country "Country")
  (column (integer-cell-format) units-sold "Units Sold")
  (column (integer-cell-format) manufacturing-price "Manuf. Price")
  (column (integer-cell-format) sale-price "Sale Price")
  (column (integer-cell-format) sales "Sales")
  (column (integer-cell-format) profit "Profit"))
