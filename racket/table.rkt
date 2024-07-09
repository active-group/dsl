#lang racket
; Tabellenformat

; Datentypen in den einzelnen Zellen

; Datensatz pro Zeile / pro ??? gibt's auch in rechteckig

; Header/Leerzeile (oder nicht)

; Liste, da: Richtung

; fehlt: räumliche Anordnung

; Eine Tabelle(nformat) ist eins der folgenden:
; - eine einzelne Zelle -ODER-
; - ein Datensatz bestehend aus mehreren Zellen -ODER-
; - Folge aus beliebig vielen Datensätzen

(struct profitability
  (segment
   country
   units-sold
   manuf-price
   sale-price
   sales
   profit)
  #:transparent)
