#lang racket/base

(define (get-info data)
  (lambda (key default)
    (case key
      ((configure-runtime)
       '(#(tables/lang/configure-runtime configure #f)))
      (else
       default))))
(provide get-info)

