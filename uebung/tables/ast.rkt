#lang racket

(struct program (format record columns) #:transparent)
(struct column (format identifier header) #:transparent)
(provide (struct-out program))
(provide (struct-out column))