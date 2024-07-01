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

#;(above
 (beside star1 circle1)
 (beside circle1 star1))

#;(above
 (beside circle1 square1)
 (beside square1 circle1))

#;(define tile
  (lambda (image1 image2) ; Abstraktion
    (above
     (beside image1 image2)
     (beside image2 image1))))

; syntaktischer Zucker
(define (tile image1 image2)
  (above
   (beside image1 image2)
   (beside image2 image1)))

; zusammengesetzte Daten

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot -UND-
; - Gewicht

; Lebendig -ODER- tot:
; - 'alive -ODER-
; - 'dead

; "Record"
(struct dillo ; Konstruktor
  (liveness ; dillo-liveness - Selektor
   weight) ; dillo-weight
  #:transparent)

; Selektoren: dillo-liveness, dillo-weight

; lebendiges Gürteltier, 10kg
(define dillo1 (dillo 'alive 10))
; totes Gürteltier, 8kg
(define dillo2 (dillo 'dead 8))

; Gürteltier überfahren
(define (run-over-dillo d)
  (dillo 'dead (dillo-weight d)))

(define (feed-dillo d amount)
  (dillo
   (dillo-liveness d)
   (match (dillo-liveness d)
     ('alive (+ (dillo-weight d) amount))
     ('dead (dillo-weight d)))))
