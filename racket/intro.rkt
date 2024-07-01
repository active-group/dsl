#lang racket
(require 2htdp/image)

(define x
  (+ 12
     (* 3 5)))

(define y (* x 3))

(define circle1 (circle 50 "solid" "gold"))
(define square1 (square 100 "solid" "blue"))
(define star1 (star 50 "solid" "red"))

(define overlay1 (overlay star1 circle1))
(define image1 (beside overlay1 square1))

; Zeilenkommentar