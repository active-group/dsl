#lang racket
(require "macros.rkt")
(define a 5)
(define b 7)
(swap! a b)
a
b
