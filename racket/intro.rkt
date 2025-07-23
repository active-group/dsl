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