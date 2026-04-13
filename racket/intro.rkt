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


; Klapperschlange hat folgende Eigenschaften:
; - Dicke   -UND-
; - Länge
(struct snake
  (thickness
   length)
  #:transparent)

; Schlange, 10cm dick, 300cm lang
(define snake1 (snake 10 300))

; Tier überfahren
(define (run-over-animal a)
  ; Summe, 2 Fälle
  (match a
    ; 2 Zweige
    ((dillo liveness weight) (dillo 'dead weight))
    ((snake thickness length) (snake 0 length))))

; Ein Duschprodukt ist eins der folgenden:
; - Seife ODER
; - Shampoo ODER
; - Mixtur bestehend aus zwei Duschprodukten

; - --Duschgel--

; Seife hat folgende Eigenschaften:
; - pH-Wert
(struct soap
  (pH)
  #:transparent)

(define soap1 (soap 7))

; Shampoo hat folgende Eigenschaften:
; - Haartyp
(struct shampoo
  (hairtype)
  #:transparent)

(define shampoo1 (shampoo 'dandruff))
(define shampoo2 (shampoo 'oily))

; Duschgel besteht aus:
; - Seife -UND-
; - Shampoo
(struct showergel
  (soap
   shampoo)
  #:transparent)

#;(define gel1 (showergel soap1 shampoo1))

; Mixtur besteht aus:
; - Duschprodukt UND
; - nochm Duschprodukt
(struct mixture
  (product1 ; Selbstbezug
   product2)
  #:transparent)

(define gel1 (mixture soap1 shampoo1))
(define gel2 (mixture gel1 shampoo2))

; Seifenanteil berechnen
(define (soap-proportion product)
  (match product
    ((soap pH) 1)
    ((shampoo hairtype) 0)
    ((mixture product1 product2)
     (/ (+ (soap-proportion product1)
           (soap-proportion product2))
        2))))

; Der Einflußbereich eines Schiffs ("shape", "area") ist eins der folgenden:
; - Kreis
; - Quadrat
; - eine Überlagerung zweier Einflußbereiche

; 1. Datenmodell dafür
; 2. Funktion, die feststellt, ob ein Punkt innerhalb oder außerhalb eines Einflußbereichs ist
