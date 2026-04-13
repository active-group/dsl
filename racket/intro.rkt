#lang racket
(require 2htdp/image)

; Kommentar

(define x
  (+ 23
     (* 2
        42)))

; Symbol (vorläufig für Aufzählungen): 'outline, 'solid

(define circle1 (circle 50 'outline "blue"))
(define square1 (square 100 'solid "red"))
(define star1 (star 50 'solid "gold"))

(define overlay1 (overlay star1 circle1))

#;(above
   (beside circle1 star1)
   (beside star1 circle1))

#;(above
   (beside square1 circle1)
   (beside circle1 square1))

; Abstraktion
; - >=2 ähnliche Codestellen
; - kopieren (letztes Mal)
; - Unterschiede durch (abstrakte) Namen ersetzen
; - Namen mit lambda-Ausdruck binden
#;(define tile
    (lambda (image1 image2)
      (above
       (beside image1 image2)
       (beside image2 image1))))

; syntaktischer Zucker:
(define (tile image1 image2)
  (above
   (beside image1 image2)
   (beside image2 image1)))

; Tier auf dem texanischen Highway ist eins der folgenden:
; - Gürteltier  -ODER-
; - Klapperschlange
; Fallunterscheidung / Summe

; Konstruktionsanleitungen (https://www.deinprogramm.de/)

; Datendefinition

; Gürteltier hat folgende Eigenschaften:
; - lebendig -ODER- tot      -UND-
; - Gewicht
; zusammengesetzte Daten / Produkt
(struct dillo
  (liveness ; 'alive oder 'dead
   weight) ; Zahl
  #:transparent)

; liefert:
; - Konstruktor dillo
; - Selektoren/"Getter" dillo-liveness, dillo-weight

; lebendiges Gürteltier, 10kg
(define dillo1 (dillo 'alive 10))
; totes Gürteltier, 8kg
(define dillo2 (dillo 'dead 8))

; Gürteltier überfahren
; "dillo rein, dillo raus"
(define (run-over-dillo* d)
  (dillo 'dead (dillo-weight d)))

(define (run-over-dillo d)
  (match d
    ((dillo l w) (dillo 'dead w))))

(module+ test
  (require rackunit)
  (check-equal? (run-over-dillo dillo1)
                (dillo 'dead 10)))

(module+ test
  (check-equal? (run-over-dillo dillo2)
                dillo2))

; Gürteltier füttern
; "dillo, dillo raus"
(define (feed-dillo* d amount)
  (dillo
   (dillo-liveness d)
   (match (dillo-liveness d)
     ; 2 Fälle, 'alive / 'dead
     ('alive (+ (dillo-weight d) amount))
     ('dead (dillo-weight d)))))

(define (feed-dillo d amount)
  (match d
    ((dillo 'alive weight) (dillo 'alive (+ weight amount)))
    ((dillo 'dead weight) d)))

(module+ test
  (check-equal?
   (feed-dillo dillo1 5)
   (dillo 'alive 15)))

