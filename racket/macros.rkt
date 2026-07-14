#lang racket
(define list1 (list 'one 'two 'three))

#;(define-syntax-rule (destructure list-expression (var1 var2 var3) body)
  (let ((var1 (first list-expression))
        (var2 (second list-expression))
        (var3 (third list-expression)))
    body))

#;(define-syntax-rule (destructure list-expression (var1 var2 var3) body)
  (let ((l list-expression))
    (let ((var1 (first l))
          (var2 (second l))
          (var3 (third l)))
        body)))

#;(define-syntax-rule (destructure list-expression (var1 var2 var3) body)
  (let ((l list-expression))
    (apply (lambda (var1 var2 var3)
             body)
           l)))

(define-syntax-rule (destructure list-expression (var1 ...) body)
  (let ((l list-expression))
    (define (f var1 ...) body)
    (apply f l)))

(define (fibonacci n)
  (match n
    (0 1)
    (1 1)
    (_ (+ (fibonacci (- n 1))
          (fibonacci (- n 2))))))

#;(destructure (map fibonacci (list 15 25 35))
             (el1 el2 el3)
             (+ el1 el2 el3))

(destructure list1
   (el1 el2 el3)
     (list 'tick el1 'trick el2 'track el3))

; Hygiene
(let ((l 'mike))
  (destructure list1
               (el1 el2 el3)
               (list l 'tick el1 'trick el2 'track el3 l)))

(let ((el1 (first list1))
      (el2 (second list1))
      (el3 (third list1)))
  (list 'tick el1 'trick el2 'track el3))
           

#;(let ((x 1)
      (y 2)
      (z 3))
  (+ x y z))

(define-syntax if*
  (syntax-rules (then else) ; Keywords
    ((if* c then k else a)
     (if c k a))))




(if* #t then 1 else 2)
(if* #f then 1 else 2)