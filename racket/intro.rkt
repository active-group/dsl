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

#;(above
 (beside star1 circle1)
 (beside circle1 star1))

#;(above
 (beside square1 star1)
 (beside star1 square1))

(define (tile image1 image2)
  (above
   (beside image1 image2)
   (beside image2 image1)))

; Tiere auf dem texanischen Highway
; - Gürteltier -ODER-
; - Klapperschlange
; Fallunterscheidung / Summe

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

; Gürteltier überfahren
(define (run-over-dillo d) ; Funktion mit dillo als Parameter
  (dillo 'dead (dillo-weight d))) ; lexikalische Bindung: von innen nach außen
  
; Gürteltier füttern
(define (feed-dillo* d amount)
  (dillo
   (dillo-liveness d)
   (match (dillo-liveness d)
     ; ein Zweig pro Fall (<Pattern> <Ergebnis>)
     ('alive (+ (dillo-weight d) amount))
     ('dead (dillo-weight d)))))
  
(define (feed-dillo d amount)
  (match d
    ((dillo 'alive w) (dillo 'alive (+ w amount)))
    ((dillo 'dead w) d)))

(struct snake
  (thickness length))

(define snake1 (snake 10 300)) ; 10cm dick, 300cm lang

#| Haskell
data Animal =
   Dillo Liveness Weight
 | Snake Length Length
|#

(define run-over-animal
  (lambda (animal)
    (match animal
      ((dillo liveness weight)
       (dillo 'dead weight))
      ((snake thickness length)
       (snake 0 length)))))
  