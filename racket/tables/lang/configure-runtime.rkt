#lang racket/base

(define (configure data)
  (current-read-interaction the-read))
(provide configure)

(require "../parser.rkt"
         "../compiler.rkt")

(define (the-read src ip)
  (cond
    ((or (not (char-ready? ip))
         (eof-object? (peek-char ip)))
     eof)
    (else
     (compile-table-definition
      (parameterize ((current-source-name src))
        (parse ip))))))
