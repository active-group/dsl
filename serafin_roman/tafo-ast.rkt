#lang racket

(require racket/contract
         racket/match)

(define srcloc/c
  (or/c #f
        (list/c any/c
                (or/c exact-positive-integer? #f)
                (or/c exact-nonnegative-integer? #f)
                (or/c exact-positive-integer? #f)
                (or/c exact-nonnegative-integer? #f))))

(define-struct predicate-sym (srcloc sym) #:prefab)
(define datum/c (or/c string? symbol? predicate-sym?))
(define (datum-equal? x y)
  (match* (x y)
    [((predicate-sym _ x) y)
     (datum-equal? x y)]
    [(x (predicate-sym _ y))
     (datum-equal? x y)]
    [(x y)
     (equal? x y)]))

(struct program (srcloc pairs)#:transparent)
(struct pair (srcloc label type)#:transparent)

(provide
 program
 pair)