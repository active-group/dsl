#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens tokens-with-payload ( #| ... |# ))
(define-empty-tokens plain-tokens ( #| ... |# EOF))

(define-lex-abbrev line-break #\newline)

; parse string literal after opening quote
(define string-lexer
  (lexer
   ((eof) (error 'tables-lexer "Unterminated string"))
   ((:~ #\" #\\ #\newline) (cons (car (string->list lexeme))
                                 (string-lexer input-port)))
   ((:: #\\ #\\) (cons #\\ (string-lexer input-port)))
   ((:: #\\ #\newline) (cons #\newline (string-lexer input-port)))
   ((:: #\\ #\") (cons #\" (string-lexer input-port)))
   (#\" '())))

(define (get-string input-port)
  (list->string (string-lexer input-port)))

; https://github.com/racket/datalog/blob/master/private/lex.rkt
