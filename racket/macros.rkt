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