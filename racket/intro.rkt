#lang racket
(require 2htdp/image)

(define x
  (+ 12
     (* 3 5)))

(define y (* x 3))

(define circle1 (circle 50 "solid" "gold"))