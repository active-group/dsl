#lang racket
(struct point
  (x y)
  #:transparent)

(struct circle
  (center radius)
  #:transparent)

(struct square
  (ll-corner side-length)
  #:transparent)

(struct overlay
  (shape1 shape2)
  #:transparent)

(define (is-in? p s)
  (match p
    ((point x y)
     (match s
       ((circle center radius)
        (<= (distance center p) radius))
       ((square (point sx sy) side-length)
        (and (>= x sx)
             (>= y sy)
             (<= x (+ sx side-length))
             (<= y (+ sy side-length))))
       ((overlay shape1 shape2)
        (or (is-in? p shape1)
            (is-in? p shape2)))))))

(define (sqr x) (* x x))

(define (distance p1 p2)
  (match p1
    ((point x1 y1)
     (match p2
       ((point x2 y2)
        (sqrt (+ (sqr (- x1 x2))
                 (sqr (- y1 y2)))))))))
            

             
