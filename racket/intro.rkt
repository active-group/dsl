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

; Klapperschlangen haben folgende Eigenschaften:
; - Länge -UND-
; - Dicke
(struct snake
  (length
   thickness)
  #:transparent)

; Prädikat: snake?

(define snake1 (snake 1.50 0.05))
(define snake2 (snake 2.50 0.02))

(define (run-over-snake s)
  (snake (snake-length s) 0))

; Ein Tier (auf dem texanischen Highway) ist eins der folgenden:
; - Gürteltier -ODER-
; - Klapperschlange

; Tier überfahren
#;(define (run-over-animal a)
  (cond ; Verzweigung
    ((dillo? a) (run-over-dillo a))  ; Zweig: (<Bedingung> <Ergebnis>)
    ((snake? a) (run-over-snake a))))

(define (run-over-animal a)
  (match a
    ((dillo liveness w)
     (dillo 'dead w))
    ((snake length thickness)
     (snake length 0))))

#;(define (feed-animal a amount)
  (match a
    ((dillo liveness weight)
     (match liveness
       ('alive (dillo 'alive (+ weight amount)))
       ('dead a)))
    ((snake length thickness)
     (snake length (+ thickness amount)))))

(define (feed-animal a amount)
  (match a
    ((dillo 'alive weight) (dillo 'alive (+ weight amount)))
    ((dillo 'dead weight) a)
    ((snake length thickness)
     (snake length (+ thickness amount)))))

; Seife ... ph-Wert -ODER-
; Shampoo ... Haartyp -ODER-
; Duschgel aus gleichen Teilen Seife und Shampoo
(struct soap
  (pH)
  #:transparent)

(define soap1 (soap 5.0))
(define soap2 (soap 6.0))

(struct shampoo
  (hairtype)
  #:transparent)

(define shampoo1 (shampoo 'dandruff))
(define shampoo2 (shampoo 'dry))

(struct showergel
  (soap shampoo)
  #:transparent)

(define gel1 (showergel soap1 shampoo1))

; ... gibt es naheliegende Verallgemeinerungen?

; Ein Duschprodukt ist eins der folgenden:
; - Seife -ODER-
; - Shampoo -ODER-
; - Mixtur aus zwei Duschprodukten
;                   ^^^^^^^^^^^^^^

(struct mixture
  (product1 product2)
  #:transparent)
