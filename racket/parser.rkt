#lang racket/base
(require parser-tools/lex
         parser-tools/yacc
         racket/list
         racket/contract
         "lexer.rkt"
         (prefix-in ast: "ast.rkt"))

(define current-source-name (make-parameter #f))

(define (make-srcloc start-pos end-pos)
  (srcloc (current-source-name) 
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos))))
