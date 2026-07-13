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

; Tier (auf dem texanischen Highway) ist eins der folgenden:
; - Gürteltier    -ODER-
; - Schlange
; ^^^^ Summe / gemischte Daten / sealed interface

; Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot    -UND-
; - Gewicht
; ^^^ Produkt / zusammengesetzte Daten / Komposition
(struct dillo ; Konstruktor
  (liveness ; 'alive   oder   'dead
   weight)
  #:transparent)

; dillo-liveness, dillo-weight: Selektoren

; lebendiges Gürteltier 10kg
(define dillo1 (dillo 'alive 10))
; totes Gürteltier 8kg
(define dillo2 (dillo 'dead 8))

; Gürteltier überfahren
; Gürteltier rein, Gürteltier raus
(define (run-over-dillo* d)
  (dillo 'dead (dillo-weight d)))

(define (run-over-dillo d)
  (match d
    ((dillo liveness weight) (dillo 'dead weight))
    ))

(module+ test
  (require rackunit)
  (check-equal? (run-over-dillo dillo1)
                (dillo 'dead 10))
  (check-equal? (run-over-dillo dillo2)
                dillo2))

; Gürteltier füttern:
; - Futtermenge Parameter
; - tote Gürteltiere nehmen nicht zu‘
#;(define (feed-dillo d amount)
  (match d
    ((dillo liveness weight)
     (match liveness
       ('alive (dillo 'alive (+ weight amount)))
       ('dead d)))))

#;(define (feed-dillo d amount)
  (define liveness (dillo-liveness d))
  (define weight (dillo-weight d))
  (match liveness
    ('alive (dillo 'alive (+ weight amount)))
    ('dead d)))

(define (feed-dillo d amount)
  (define liveness (dillo-liveness d))
  (define weight (dillo-weight d))
  (if (equal? liveness 'alive)
      ; Konsequente, "then"
      (dillo 'alive (+ weight amount))
      ; Alternative, "else"
      d))

(module+ test
  (check-equal? (feed-dillo dillo1 5)
                (dillo 'alive 15))
  (check-equal? (feed-dillo dillo2 5)
                dillo2))


; Schlangen:
; - Dicke  -UND-
; - Länge
(struct snake
  (thickness
   length)
  #:transparent)

; Schlange  10cm dick, 3m lang
(define snake1 (snake 10 300))
; Schlange 5cm dick, 1m lang
(define snake2 (snake 5 100))

; Tier überfahren
(define (run-over-animal a)
  (match a
    ; 1 Zweig pro Fall / Summand
    ((dillo liveness weight)
     (dillo 'dead weight))
    ((snake thickness length)
     (snake 0 length))))


; Duschprodukt ist eins der folgenden:
; - Seife  ODER
; - Shampoo ODER
; - Mixtur bestehend aus Duschprodukt und noch nem Duschprodukt
;                        ^^^^^^^^^^^^ Selbstreferenz

; - Seife
(struct soap
  (pH)
  #:transparent)

(define soap1 (soap 7))
(define soap2 (soap 5))

; - Shampoo
(struct shampoo
  (hairtype)
  #:transparent)

(define shampoo1 (shampoo 'oily))
(define shampoo2 (shampoo 'dandruff))

; - Duschgel bestehend aus Seife UND Shampoo
(struct showergel
  (soap
   shampoo)
  #:transparent)

(define gel1 (showergel soap1 shampoo1))

; Kombinator
(struct mixture
  (product1
   product2)
  #:transparent)

(define mixture1 (mixture soap1 shampoo1))
(define mixture2 (mixture mixture1 shampoo2))

; Seifenanteil
(define (soap-proportion product)
  (match product
    ((soap pH) 1)
    ((shampoo hairtype) 0)
    ((mixture product1 product2)
     (/ (+
         (soap-proportion product1)
         (soap-proportion product2))
        2))))

(module+ test
  (check-equal? (soap-proportion mixture1)
                1/2)
  (check-equal? (soap-proportion mixture2)
                1/4))


; Eine geometrische Figur ("Shape") ist eins der folgenden:
; - Kreis
; - Quadrat
; - eine Überlagerung zweier geometrischer Figuren

; 1. Datenmodellierung dafür
; 2. Funktion, die herausbekommt, ob ein Punkt innerhalb oder außerhalb einer Figur liegt