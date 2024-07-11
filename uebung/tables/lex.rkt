#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens dtokens (KEY IDENTIFIER STRING))
(define-empty-tokens dpunct (LPAREN COMMA RPAREN DATA
                                    FORMAT RECORD COLUMN EOF))
(define-lex-abbrev id-chars (char-complement (char-set "(,)=:.~?\"% \n")))
(define-lex-abbrev identifier-re (:: id-chars (:* (:or upper-case id-chars))))
(define-lex-abbrev string-chars (char-complement (char-set "\"")))
(define-lex-abbrev string-re (:: #\" (:* string-chars) #\"))
(define-lex-abbrev key-re (:: identifier-re #\:))


(define dlexer
  (lexer-src-pos
   [whitespace
    (return-without-pos (dlexer input-port))]
   ["default-data-set:" (token-DATA)]
   ["format:" (token-FORMAT)]
   ["record:" (token-RECORD)]
   ["column:" (token-COLUMN)]
   [identifier-re
    (token-IDENTIFIER lexeme)]
   [string-re
    (token-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [#\( (token-LPAREN)]
   [#\, (token-COMMA)]
   [#\) (token-RPAREN)]
   [(eof) (token-EOF)]))

(provide dtokens dpunct
         id-chars string-re identifier-re
         dlexer)

(module+ test
  (require rackunit)
  (define (test-lexer str tok-name [tok-value str])
    (define pv (dlexer (open-input-string str)))
    (define v (position-token-token pv))
    (test-equal? (format "lexer: ~a: <~a,~a>" str tok-name tok-value)
                 (cons (token-name v) (token-value v))
                 (cons tok-name tok-value)))
  (define (get-tokens input-port)
    (let ((with-position (lambda () (dlexer input-port))))
      (let loop ((tokens '()))
        (let ((token (position-token-token (with-position))))
          (if (eq? (token-name token) 'EOF)
              (reverse tokens)
              (loop (cons token tokens)))))))

  (define (get-tokens-from-file filename)
    (call-with-input-file filename get-tokens))
  
  (test-lexer "(" 'LPAREN #false)
  (test-lexer ")" 'RPAREN #false)
  (test-lexer "," 'COMMA #false)
  (test-lexer "-abc" 'IDENTIFIER "-abc")
  (test-lexer "\"test\"" 'STRING "test")
  (get-tokens-from-file "test.rkt")
  )