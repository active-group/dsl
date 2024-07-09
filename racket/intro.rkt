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
#;(define tile
  (lambda (image1 image2)
    (above
     (beside image1 image2)
     (beside image2 image1))))

(define image1 circle1)

; statische / lexikalische Bindung

(define (tile image1 image2) ; syntaktischer Zucker
  (above
   (beside image1 image2)
   (beside image2 image1)))

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
; die Repräsentation
; des Zustands des Gürteltiers zu einem bestimmten Zeitpunkt

; lebendiges Gürteltier, 10kg
(define dillo1 (dillo 'alive 10))
; totes Gürteltier, 8kg
(define dillo2 (dillo 'dead 8))

; Gürteltier überfahren
(define (run-over-dillo d)
  (dillo 'dead
   (dillo-weight d)))

(module+ test
  (require rackunit)

  (check-equal? (run-over-dillo dillo1)
                (dillo 'dead 10))
  (check-equal? (run-over-dillo dillo2)
                dillo2)
  
  )

; Klapperschlange hat folgende Eigenschaften:
; - Länge -UND-
; - Dicke
(struct rattlesnake
  (length
   thickness)
  #:transparent)

(define snake1 (rattlesnake 150 7))
(define snake2 (rattlesnake 70 3))

(define (run-over-rattlesnake s)
  (rattlesnake (rattlesnake-length s)
               0))

(module+ test
  (check-equal? (run-over-rattlesnake snake1)
                (rattlesnake 150 0)))

; Tier ist eins der folgenden:
; - Gürteltier -ODER-
; - Klapperschlange
; Fallunterscheidung, hier: gemischte Daten

; Prädikat: dillo? rattlesnake?

#;(define (run-over-animal a)
  ; Verzweigung, 1 Zweig pro Fall
  (cond
    ((dillo? a) (run-over-dillo a)) ; (<Bedingung> <Ergebnis>)
    ((rattlesnake? a) (run-over-rattlesnake a))))

; Pattern-Matching
(define (run-over-animal a)
  (match a ; Verzweigung
    ((dillo l w)
     (dillo 'dead w))
    ((rattlesnake l _) ; _ : "don't care"
     (rattlesnake l 0))))

; Tier füttern:
; Funktion: Tier rein, Tier rein
; - Gürteltier wird schwerer, wenn es noch lebt
; - Klapperschlange wird dicker