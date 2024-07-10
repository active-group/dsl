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
; "von innen nach außen"

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
(define (feed-animal a)
  (match a
    ((dillo liveness weight)
     (match liveness
       ('alive (dillo 'alive (+ weight 1)))
       ('dead a)))
    ((rattlesnake length thickness)
     (rattlesnake length (+ thickness 1)))))

; Blumen

; Einzelne Blume hat folgende Eigenschaften:
; - Farbe -UND-
; - Form -UND-
; - Sorte
(struct single-flower
  (color
   form
   sort)
  #:transparent)

(define rose1 (single-flower 'red 'round 'rose))
(define tulip1 (single-flower 'yellow 'tapered 'tulip))

; Wir wollen rose1 und tulip1 in einem Strauß kombinieren
(struct two
  (bouqet1
   bouqet2)
  #:transparent)

; Zwei Blumen
(define bouquet1 (two rose1 tulip1))

(define violet1 (single-flower 'violet 'square 'violet))

; Strauß mit drei Blumen
(define bouquet2 (two bouquet1 violet1))

(struct empty-bouquet () #:transparent)
(define the-empty-bouquet (empty-bouquet))

; Ein Blumenstrauß ist eins der folgenden:
; - der leere Blumenstrauß
; - eine einzelne Blume -ODER-
; - eine Zweier-Kombi aus zwei Blumensträußen
;                              ^^^^^^^^^^^^^^
;                              Selbstbezug

; (keine 2-Klassen-Gesellschaft)

; Strauß aus nur den Rosen
(define (only-roses-bouquet bouquet)
  (match bouquet
    ((single-flower color form sort)
     (match sort
       ('rose bouquet)
       (_ the-empty-bouquet)))
    ((two bouquet1 bouquet2)
     (two
      (only-roses-bouquet bouquet1) ; 1. Selbstbezug
      (only-roses-bouquet bouquet2) ; 2. Selbstbezug
     ))))

; allgemein:
; Eine Liste ist eins der folgenden:
; - die leere Liste -ODER-
; - eine Cons-Liste aus erstem Element und Rest-Liste
;                                               ^^^^^


; In Racket:
; - leere Liste: '()
; - Cons-Liste: (cons f r)

; 1elementige Liste: 4
(define list1 (cons 4 '()))
; 2elementige Liste: 3 4
(define list2 (cons 3 (cons 4 '())))
; 3elementige Liste: 8 3 4
(define list3 (cons 8 list2))
; 4elementige Liste 5 8 3 4
(define list4 (cons 5 list3))

; Elemente einer Liste aufsummieren
(define (list-sum list)
  (match list
    ('() 0)
    ((cons first rest)
     (+ first
        (list-sum rest)))))

(define sum
  (lambda list
    (list-sum list)))

#;(define (sum . list)
  (list-sum list))

(define (list-sum* list)
  (apply + list))