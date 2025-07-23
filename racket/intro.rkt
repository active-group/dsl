#lang racket
(require 2htdp/image)

(define x
  (+ 12 (* 34 12)))

(define circle1
  (circle 50 "solid" "red"))
(define square1
  (square 100 "outline" "blue"))
(define star1
  (star 50 "solid" "gold"))

; overlay: 2 Bilder rein, 1 Bild raus
(define overlay1
  (overlay star1 circle1))
(define p1
  (beside overlay1 square1))

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot    -UND-
; - Gewicht
; zusammengesetzte Daten / Produkt
(struct dillo
  (liveness ; 'alive -ODER- 'dead
   weight)
  #:transparent) ; damit die Structs ausgedruckt werden

; lebendiges Gürteltier, 10kg
(define dillo1 (dillo 'alive 10))
; totes Gürteltier, 8kg
(define dillo2 (dillo 'dead 8))