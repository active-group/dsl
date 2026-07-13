#lang racket
(require 2htdp/image) ; "How to do Design Programs"

; Mike sein Code
(define x
  (+ 23
     (* 2
        21)))

(define circle1 (circle 50 'solid "red"))

(define square1 (square 100 'outline "gold"))

(define star1 (star 50 'solid "green"))

(define y 'mike) ; Symbol: "enum"

(define o1 (overlay star1 circle1))

#;(above
 (beside star1 circle1)
 (beside circle1 star1))

#;(above
 (beside circle1 square1)
 (beside square1 circle1))

; Abstraktion: 2 Code-Fragmente, sehr ähnlich
; 1. (ein letztes Mal) kopieren
; 2. Unterschiede ersetzen durch (abstrakte) Namen
; 3. neue Namen in lambda aufnehmen

#;(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))


(define (tile image1 image2) ; syntaktischer Zucker
  (above
   (beside image1 image2)
   (beside image2 image1)))

; Tiere auf dem texanischen Highway

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot    -UND-
; - Gewicht
; ^^^ Produkt / zusammengesetzte Daten / Komposition
(struct dillo
  (liveness ; 'alive   oder   'dead
   weight)
  #:transparent)

; lebendiges Gürteltier 10kg
(define dillo1 (dillo 'alive 10))
; totes Gürteltier 8kg
(define dillo2 (dillo 'dead 8))

