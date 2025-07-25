#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens tokens-with-payload (IDENTIFIER STRING))
(define-empty-tokens plain-tokens (LPAREN RPAREN COMMA DEFINE TABLE WITH STR INT CURRENCY EOF))
(define-lex-abbrev id-chars (char-complement (char-set "(,)=:.~?\"% \n")))
(define-lex-abbrev identifier-re (:: id-chars (:* (:or upper-case id-chars))))


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

#|
  DEFINE TABLE t3 WITH StructName (STR segment "Segment", STR country "Country")

  (get-tokens-from-string "DEFINE TABLE t3 WITH StructName (STR segment \"Segment\", STR country \"Country\")")
  |#

(define dlexer
  (lexer-src-pos
   [whitespace
    (return-without-pos (dlexer input-port))]
   [#\" (token-STRING (get-string input-port))]
   [#\( (token-LPAREN)]
   [#\, (token-COMMA)]
   [#\) (token-RPAREN)]
   ["DEFINE" (token-DEFINE)]
   ["TABLE" (token-TABLE)]
   ["WITH" (token-WITH)]
   ["STR" (token-STR)]
   ["INT" (token-INT)]
   ["CURRENCY" (token-CURRENCY)]
   [identifier-re
    (token-IDENTIFIER lexeme)]
   [(eof) (token-EOF)]))
   

; https://github.com/racket/datalog/blob/master/private/lex.rkt

(define (get-tokens input-port)
  (let ((with-position (lambda () (dlexer input-port))))
    (let loop ((tokens '()))
      (let ((token (position-token-token (with-position))))
        (if (eq? (token-name token) 'EOF)
            (reverse tokens)
            (loop (cons token tokens)))))))

(define (get-tokens-from-file filename)
  (call-with-input-file filename get-tokens))

(define (get-tokens-from-string string)
  (get-tokens (open-input-string string)))
