#lang racket
; Tabellenformat

; Datentypen in den einzelnen Zellen

; Datensatz pro Zeile / pro ??? gibt's auch in rechteckig

; Header/Leerzeile (oder nicht)

; Liste, da: Richtung

; fehlt: räumliche Anordnung

(struct profitability
  (segment
   country
   units-sold
   manuf-price
   sale-price
   sales
   profit)
  #:transparent)
