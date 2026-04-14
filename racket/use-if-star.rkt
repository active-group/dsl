#lang racket
(require (only-in "macros.rkt" if*))

(define then 'egal)

(if* #t then 1 else 2)