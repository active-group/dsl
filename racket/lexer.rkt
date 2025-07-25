#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens tokens-with-payload ( #| ... |# ))
(define-empty-tokens plain-tokens ( #| ... |# EOF))

(define-lex-abbrev line-break #\newline)

; https://github.com/racket/datalog/blob/master/private/lex.rkt
