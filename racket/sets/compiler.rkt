#lang racket
(require "ast.rkt")

(define (compile ast)
  (match ast
    ((set-assignment srcloc identifier definition)
     `(define ,identifier ,(compile-definition definition)))))

(define (compile-definition d)
  (match d
    ((set-definition srcloc body)
     (match body
       ((list elements ...)
        `',elements)
       ((constrained-set-definition srcloc element-definition list-set-constraints)
        'fixme)))))

(compile
 (set-assignment #f 'name
                 (set-definition #f '(1 2 3 4 5 6))))