#lang racket
(require 2htdp/image)

(define x
  (+ 12
     (* 5
        4)))

(define circle1 (circle 50 "solid" "yellow"))
(define square1 (square 100 "outline" "green"))
(define star1 (star 50 "solid" "red"))

(define overlay1 (overlay star1 circle1))

; overlay, beside, above: Kombinatoren

#;(above
 (beside circle1 star1)
 (beside star1 circle1))

#;(above
 (beside square1 circle1)
 (beside circle1 square1))

; Abstraktion
(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

; Tiere auf dem texanischen Highway ...

; Gürteltier hat folgende Eigenschaften:
; - Lebendigkeit            -UND-
; - Gewicht
; zusammengesetzte Daten

; Lebendigkeit / "liveness"
; lebendig -ODER- tot
; allgemein: Fallunterscheidung, hier: Aufzählung
; benutzen Symbole 'alive, 'dead

(struct dillo ; Konstruktor
  (liveness ; Feld, Selektor/Getter: dillo-liveness
   weight)
  #:transparent)

; lebendiges Gürteltier, 10kg
(define dillo1 (dillo 'alive 10))
; totes Gürteltier, 8kg
(define dillo2 (dillo 'dead 8))

