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
(define (table-with-header-row header+types constructor)
  (Rowdefinition 'vertical
                 (list
                  (Rowdefinition 'horizontal
                                 (map Header (map header+type-header header+types))
                                 constructor)
                  (Tabledef 'vertical
                            (Rowdefinition 'horizontal
                                           (map Cell (map header+type-type header+types)))
                            constructor))
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
         ((cons t rest) (cons (parse-tcontents t tcontents x y) (recurse rest
                                                                         (if (equal? direction 'horizontal) (+ 1 x) x)
                                                                         (if (equal? direction 'vertical) (+ 1 y) y)
                                                                         )))
         ))
     (apply constructor (recurse lot x y)))
    ((Tabledef direction content)
     (define (recurse x y)
       (with-handlers
           ((exn:fail? (lambda (exn) '())))
         (cons (parse-tcontents content tcontents x y)
               (recurse (if (equal? direction 'horizontal) (+ 1 x) x)
                        (if (equal? direction 'vertical) (+ 1 y) y)))))
     (recurse x y))
       
    ))



(define currency? string?)


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
           (list (header+type "Segment" 'string)
                 (header+type "Country" 'string)
                 (header+type "Units Sold" 'int)
                 (header+type "Manuf. Price" 'currency)
                 (header+type "Sale Price" 'currency)
                 (header+type "Sales" 'currency)
                 (header+type "Profit" 'currency))
           CellOutput))
                 
t


(parse-tcontents rowdefinition (list->tcontents '(("Government" "Canada" 1618 "$3,00" "$20,00" "$32.370,00" "$16.185,00"))) 0 0)
    
(parse-tcontents t units-tcontents 0 0)
  
