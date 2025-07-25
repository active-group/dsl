#lang racket
(provide swap!)
#;(define (swap! x y)
  (let ((z x))
    (set! x y)
    (set! y z)))

; Makro
(define-syntax-rule (swap! x y)
  (let ((z x)) ; neue Variable
    (set! x y)
    (set! y z)))

(define y 23)
(define z 42)
(swap! y z)
y
z

#;(define-syntax-rule (my-min x y)
  (let ((x* x)
        (y* y))
    (if (< x* y*)
        x*
        y*)))

(define (fib n)
  (cond
    ((= n 0) 1)
    ((= n 1) 1)
    (else (+ (fib (- n 1)) (fib (- n 2))))))

;(my-min (fib 23) (fib 50))

(define-syntax my-min
  (syntax-rules ()
    ; ein Zweig pro Fall (<Pattern> <Ergebnis>)
    ((my-min x) x)
    ((my-min x y)
     (let ((x* x)
           (y* y))
       (if (< x* y*)
           x*
           y*)))
    ((my-min x y ...) ; y ... beliebig viele ys
     (my-min x (my-min y ...)))
    #;((my-min x y z)
     (my-min x (my-min y z)))))

; Makros, die binden
(define list1 (list 1 2 3))

(define-syntax-rule (destructure list-exp (v ...) body)
  (apply (lambda (v ...)
           body)
         list-exp))


(destructure list1 (x y z)
  (+ x y z))

; -->
#;(apply (lambda (x y z)
         (+ x y z))
       list1)

(define-syntax if*
  (syntax-rules (then else) ; Literal
    ((if* condition then consequent else alternative)
     (if condition consequent alternative))))

(if* (> 2 1) then "zwo" else "uno")
;(if* (> 2 1) dann "zwo" sonst "uno")

#|
triangles =
      [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]

rightTriangles =
      [ (a,b,c) |
        c <- [1..10],
        b <- [1..c],
        a <- [1..b],
        a^2 + b^2 == c^2 ]
|#

(define (from-to m n)
  (if (> m n)
      '()
      (cons m (from-to (+ m 1) n))))

(define (concat-map f l)
  (apply append (map f l)))

(define-syntax ||
  (syntax-rules (<- let) ; Literale
    ((|| e #t) (list e))
    ((|| e q) (|| e q #t))
    ((|| e (p <- l) Q ...)
     (let ((ok (lambda (p)
                 (|| e Q ...))))
       (concat-map ok l)))
    ((|| e (let decls) Q ...)
     (let decls
       (|| e Q ...)))
    ((|| e b Q ...)
     (if b
         (|| e Q ...)
         '()))))
                 

(define triangles
  (|| (list a b c)
      (c <- (from-to 1 10))
      (b <- (from-to 1 10))
      (a <- (from-to 1 10))))

(define (sqr x) (* x x))

(define right-triangles
  (|| (list a b c)
      (c <- (from-to 1 10))
      (b <- (from-to 1 c))
      (a <- (from-to 1 b))
      (= (+ (sqr a) (sqr b)) (sqr c))))