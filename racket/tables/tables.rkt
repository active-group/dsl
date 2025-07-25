#lang racket

#|
Tabelle als Kombinatormodell:
    - P: EINE Zelle mit "primitivem" Typ  -ODER-
    - P: ein Header -ODER-
    - C: eine Datensatz aus einer festen Anzahl von Tabellen -> Struct/Record
    - C: eine Liste aus einer dynamischen Anzahl von gleichartigen Tabellen -> Liste

Beispiel:
Segment,Country,Units Sold,Manuf. Price,Sale Price,Sales,Profit
Government,Canada,1618,"$3,00","$20,00","$32.370,00","$16.185,00"
Government,Germany,1321,"$3,00","$20,00","$26.420,00","$13.210,00"
Midmarket,France,2178,"$3,00","$15,00","$32.670,00","$10.890,00"
Midmarket,Germany,888,"$3,00","$15,00","$13.320,00","$4.440,00"
Midmarket,Mexico,2470,"$3,00","$15,00","$37.050,00","$12.350,00"

Table:
  Cell
  | struct
  | Table Table
|#



(struct Cell
  (type); primitive type ('string, 'int)
  )

(struct Header
  (title); title of the cell
  )

(struct Tabledef
  (direction ; direction: 'horizontal | 'vertical
   content ; table
   )
  #:transparent)

(struct Rowdefinition
  (direction list constructor) ; direction: 'horizontal | 'vertical list: list of tables
  #:transparent )


; pro "Spalte": Überschrift + Typ
(struct header+type
  (header type)
  #:transparent)

; außerdem Konstruktor für den "Zeilen"-Datensatz
(define (table-with-header-row constructor . header+types)
  (Rowdefinition 'vertical
                 (list
                  (Rowdefinition 'horizontal
                                 (map Header (map header+type-header header+types))
                                 constructor)
                  (Tabledef 'vertical
                            (Rowdefinition 'horizontal
                                           (map Cell (map header+type-type header+types))
                                           constructor)))
                 (lambda (header list) list)))


; Tabelleninhalt: Funktion: x y -> Inhalt der Tabelle
(define (tcontents0 x y) ; Tablle, wo überall "Mike"
  "Mike")

(define (tcontents1 x y)
  (if (even? x)
      "Mike"
      "Tobias"))

(define units-list
  '(("Segment" "Country" "Units Sold" "Manuf. Price" "Sale Price" "Sales" "Profit")
    ("Government" "Canada" 1618 "$3,00" "$20,00" "$32.370,00" "$16.185,00")
    ("Government" "Germany" 1321 "$3,00" "$20,00" "$26.420,00" "$13.210,00")
    ("Midmarket" "France" 2178 "$3,00" "$15,00" "$32.670,00" "$10.890,00")
    ("Midmarket" "Germany" 888 "$3,00" "$15,00" "$13.320,00" "$4.440,00")
    ("Midmarket" "Mexico" 2470 "$3,00" "$15,00" "$37.050,00" "$12.350,00")))

(define (list->tcontents l)
  (lambda (x y)
    (list-ref (list-ref l y) x)))

(define units-tcontents (list->tcontents units-list))

(define (parse-tcontents table tcontents x y)
  (match table
    ((Header title) (if (equal? title (tcontents x y)) title (error "Header mismatch")))
    ((Cell type) (match type
                   ('string (if (string? (tcontents x y)) (tcontents x y) (error "Expected string")))
                   ('int (if (integer? (tcontents x y)) (tcontents x y) (error "Expected integer")))
                   ('currency (if (currency? (tcontents x y)) (tcontents x y) (error "Expected currency")))
    ))
    ((Rowdefinition direction lot constructor)
     (define (recurse lot x y)
       (match lot
         ('() (list))
         ((cons t rest)
          (cons (parse-tcontents t tcontents x y)
                (match rest
                  ('() (list)) ; Aufruf von table-height vermeiden, der bomben könnte
                  (_ (recurse rest
                              (if (equal? direction 'horizontal)
                                  (+ (table-width t) x)
                                  x)
                              (if (equal? direction 'vertical)
                                  (+ (table-height t) y)
                                  y)
                              )))))))
     (apply constructor (recurse lot x y)))
    ((Tabledef direction content)
     (define (recurse x y)
       (with-handlers
           ((exn:fail? (lambda (exn) '())))
         (cons (parse-tcontents content tcontents x y)
               (recurse (if (equal? direction 'horizontal)
                            (+ (table-width content) x)
                            x)
                        (if (equal? direction 'vertical)
                            (+ (table-height content) y)
                            y)))))
     (recurse x y))
       
    ))

(define (table-height t)
  (match t
    ((Cell type) 1)
    ((Header title) 1)
    ((Rowdefinition 'horizontal list constructor)
     (apply max (map table-height list)))
    ((Rowdefinition 'vertical list constructor)
     (apply + (map table-height list)))
    ((Tabledef 'horizontal content)
     (table-height content))
    ((Tabledef 'vertical content)
     (error 'table-height "height of a vertical Tabledef is not defined"))))

(define (table-width t)
  (match t
    ((Cell type) 1)
    ((Header title) 1)
    ((Rowdefinition 'horizontal list constructor)
     (apply + (map table-width list)))
    ((Rowdefinition 'vertical list constructor)
     (apply max (map table-width list)))
    ((Tabledef 'horizontal content)
     (error 'table-width "width of a horizontal Tabledef is not defined"))
    ((Tabledef 'vertical content)
     (table-width content))))
    
     
  

(define currency? string?)

(module+ test
  (require rackunit)
  
(struct CellOutput
  (segment country
   unitsSold manufacturingPrice
   salePrice
   sales profit)
  #:transparent)

#| Segment,Country,Units Sold,Manuf. Price,Sale Price,Sales,Profit
|#
(define headerrow
  (Rowdefinition 'horizontal (list
                              (Header "Segment")
                              (Header "Country")
                              (Header "Units Sold")
                              (Header "Manuf. Price")
                              (Header "Sale Price")
                              (Header "Sales")
                              (Header "Profit")
                              )
                 CellOutput))

(define rowdefinition
  (Rowdefinition 'horizontal
                 (list (Cell 'string) (Cell 'string)
                       (Cell 'int) (Cell 'currency)
                       (Cell 'currency)
                       (Cell 'currency)
                       (Cell 'currency))
                 CellOutput))

(define t0
  (Rowdefinition 'vertical
                 (list
                  headerrow
                  (Tabledef 'vertical rowdefinition))
                 (lambda (header list) list)))


(define t (table-with-header-row
           CellOutput
           (header+type "Segment" 'string)
           (header+type "Country" 'string)
           (header+type "Units Sold" 'int)
           (header+type "Manuf. Price" 'currency)
           (header+type "Sale Price" 'currency)
           (header+type "Sales" 'currency)
           (header+type "Profit" 'currency)))

  

  ; Aufgabe: Makro-Layer auf table-with-header-row, soll struct generieren, ohne header+type

  #|

(define-table t2 CellOutput2
   (segment "segment-string" string)

(define-table t2 CellOutput2
  (segment "Segment" string)
  (units-sold "Units Sold" currency))

(define-syntax my-min
  (syntax-rules ()
    ; ein Zweig pro Fall (<Pattern> <Ergebnis>)
    ((my-min x) x)
    ((my-min x y)
     (let ((x* x)
           (y* y))
       (if (< x* y*)
           x*
           y*)))
    ((my-min x y ...) ; y ... beliebig viele ys
     (my-min x (my-min y ...)))
    #;((my-min x y z)
     (my-min x (my-min y z)))))

(begin
  (define mike 23)
  (define sperber 42))

|#

  (define-syntax define-table
    (syntax-rules ()
      ((define-table table-name struct-name (field-name field-title field-type) ...)
       (begin
         (struct struct-name
            (field-name ...) #:transparent)
         (define table-name (table-with-header-row
           struct-name
           (header+type field-title 'field-type) ...
           )))
       )
      )
    )

(define-table t2 CellOutput2
  (segment "Segment" string)
  (units-sold "Units Sold" currency))

  t2
  (CellOutput2 "test" "test")
  
  
  ; aus zwei Definitionen eine machen
(begin
  (define mike 23)
  (define sperber 42))
  
  (check-equal?
   (parse-tcontents rowdefinition (list->tcontents '(("Government" "Canada" 1618 "$3,00" "$20,00" "$32.370,00" "$16.185,00"))) 0 0)
   (CellOutput "Government" "Canada" 1618 "$3,00" "$20,00" "$32.370,00" "$16.185,00"))
    
(check-equal? (parse-tcontents t units-tcontents 0 0)
              (list
               (CellOutput "Government" "Canada" 1618 "$3,00" "$20,00" "$32.370,00" "$16.185,00")
               (CellOutput "Government" "Germany" 1321 "$3,00" "$20,00" "$26.420,00" "$13.210,00")
               (CellOutput "Midmarket" "France" 2178 "$3,00" "$15,00" "$32.670,00" "$10.890,00")
               (CellOutput "Midmarket" "Germany" 888 "$3,00" "$15,00" "$13.320,00" "$4.440,00")
               (CellOutput "Midmarket" "Mexico" 2470 "$3,00" "$15,00" "$37.050,00" "$12.350,00")))

  
  (struct Place (country continent) #:transparent)
  (define place-table (Rowdefinition 'vertical (list (Cell 'string) (Cell 'string))
                                     Place))

  (struct Sales (place sales) #:transparent)
  (define sales-table (Rowdefinition 'horizontal (list place-table (Cell 'int)) Sales))

  (check-equal?
   (parse-tcontents sales-table
                    (list->tcontents
                     '(("Germany" 15)
                       ("Europe" "")))
                    0 0)
   (Sales (Place "Germany" "Europe") 15))

  (check-equal?
   (parse-tcontents (Tabledef 'vertical sales-table)
                    (list->tcontents
                     '(("Germany" 0)
                       ("Europe"  "")
                       ("Spain"   1)
                       ("Europe"  "")))
                    0 0)
   (list (Sales (Place "Germany" "Europe") 0) (Sales (Place "Spain" "Europe") 1)))

  
  )

#|
Place      |         | Score
|---------------------
| Germany  |  Europe | 0
----------------------
|#