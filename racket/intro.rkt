#lang racket
(require 2htdp/image)

; Kommentar

(define x
  (+ 23
     (* 2
        42)))

; Symbol (vorläufig für Aufzählungen): 'outline, 'solid

(define circle1 (circle 50 'outline "blue"))
(define square1 (square 100 'solid "red"))
(define star1 (star 50 'solid "gold"))

(define overlay1 (overlay star1 circle1))

(above
 (beside circle1 star1)
 (beside star1 circle1))

(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion
; - >=2 ähnliche Codestellen
; - kopieren (letztes Mal)
; - Unterschiede durch (abstrakte) Namen ersetzen
; - Namen mit lambda-Ausdruck binden
(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

; Tiere auf dem texanischen Highway

; Konstruktionsanleitungen (https://www.deinprogramm.de/)

; Datendefinition

; Gürteltier hat folgende Eigenschaften:
; - lebendig -ODER- tot      -UND-
; - Gewicht
; zusammengesetzte Daten / Produkt
(struct dillo
  (liveness ; 'alive oder 'dead
   weight) ; Zahl
  #:transparent)

; lebendiges Gürteltier, 10kg
(define dillo1 (dillo 'alive 10))