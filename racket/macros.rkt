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

(define-syntax-rule (my-min x y)
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
