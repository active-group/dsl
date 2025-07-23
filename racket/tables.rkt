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

; Example
(define cell1 (Cell 'string))

(struct Header
  (title); title of the cell
  )

(define header1 (Header "Segment"))


(struct Tabledef
  (direction ; direction: 'horizontal | 'vertical
   content ; table
   )
  #:transparent)

(struct Rowdefinition
  (direction list) ; direction: 'horizontal | 'vertical list: list of tables
  #:transparent )

#| Segment,Country,Units Sold,Manuf. Price,Sale Price,Sales,Profit
|#
(define headerrow (Rowdefinition 'horizontal (list
                                              (Header "Segment")
                                              (Header "Country")
                                              (Header "Units Sold")
                                              (Header "Manuf. Price")
                                              (Header "Sale Price")
                                              (Header "Sales")
                                              (Header "Profit")
                                              )))

(define rowdefinition (Rowdefinition 'horizontal (list (Cell 'string) (Cell 'string) (Cell 'int) (Cell 'currency) (Cell 'currency) (Cell 'currency) (Cell 'currency))))

(define t
  (Rowdefinition 'vertical
                 (list
                  headerrow
                  (Tabledef 'vertical rowdefinition))))

t


