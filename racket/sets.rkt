#lang racket
; Eine Menge ist eins der folgenden:
; X = { x \in A | x <= 5 }

; - eine Einschränkung einer Menge  -ODER-
; - eine Liste aller Elemente (Reihenfolge spielt keine Rolle)

(struct list-set
  (elements) ; Liste der Elemente
  #:transparent)

(struct restricted-set
  (restrictions ; Liste von Einschränkungen
   set)
  #:transparent)

; Eine Einschränkung ist ein Prädikat,
; das sagt, ob ein Element drin ist oder nicht.
; 1. Versuch: Funktion: Element rein, Boolean raus

(define set10
  (list-set (list 1 2 3 4 5 6 7 8 9 10)))

(define evens10
  (restricted-set (list even?)
                  set10))

(define set<5
  (restricted-set (list (lambda (x) (< x 5)))
                  set10))

(define set???
  (restricted-set (list (lambda (x) (< x 5)) even?)
                  set10))

; Kompositionalität:
; Die Bedeutung eines Gegenstands hängt nur von der Bedeutung
; seiner Bestandteile ab.

; Semantik:
; Elemente einer Menge
(define (set-elements set)
  (match set
    ((list-set elements) elements)
    ((restricted-set restrictions inner-set)
     (filter (lambda (element)
              (fulfills-all? restrictions element))
             (set-elements inner-set)))))

(define (fulfills-all? predicates x)
  (match predicates
    ('() #t)
    ((cons first rest)
     (and (first x)
          (fulfills-all? rest x)))))

(module+ test
  (require rackunit)
  (check-equal? (set-elements evens10)
                (list 2 4 6 8 10))
  (check-equal? (set-elements set<5)
                (list 1 2 3 4))
  (check-equal? (set-elements set???)
                (list 2 4)))
                                      