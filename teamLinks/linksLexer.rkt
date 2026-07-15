#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "ast.rkt")

#;(

Table tableName
 header : Segment, Country, Units Sold
 types: string, string, number
 fields: segment, country, units-sold

)

(define-tokens dtokens (IDENTIFIER NAME))
(define-empty-tokens dpunct (TABLE COMMA COLON EOF HEADER TYPES FIELDS))
(define-lex-abbrev line-break #\newline)

(define-lex-abbrev identifier-re
  (:: upper-case (:* (:or upper-case lower-case))))

(define-lex-abbrev name-re
  (:: (:or upper-case lower-case)
      (:* (:or upper-case lower-case (char-set "- ")))
      (:or upper-case lower-case "-")))

(define dlexer
  (lexer-src-pos
   [whitespace
    (return-without-pos (dlexer input-port))]
   [line-break
    (return-without-pos (dlexer input-port))]
   ["," (token-COMMA)]
   [":" (token-COLON)]
   ["Table" (token-TABLE)]

   ["header" (token-HEADER)]
   ["types" (token-TYPES)]
   ["fields" (token-FIELDS)]
   [identifier-re
    (token-IDENTIFIER lexeme)]
   [name-re
    (token-NAME lexeme)]
   

   [(eof) (token-EOF)]))

(provide dtokens dpunct line-break name-re dlexer)

(module+ test
  (require rackunit)

  (define (test-token input-string name [value #f])
    (let* ((with-position (dlexer (open-input-string input-string)))
           (token (position-token-token with-position)))
      (test-equal? (format "lexer: ~a: <~a,~a>" input-string name value)
                   (cons (token-name token) (token-value token))
                   (cons name value))))

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


   (test-token "," 'COMMA)
   (test-token ":" 'COLON)
  
   (test-token "Table" 'TABLE)
   (test-token "header" 'HEADER)
   (test-token "types" 'TYPES)

  
  (test-token "Foo-" 'NAME "Foo-")
  (test-token "TableName" 'IDENTIFIER "TableName")
  (test-token " TableName" 'IDENTIFIER "TableName")
  (test-token "TableName " 'IDENTIFIER "TableName")
  ; (test-token "Table TableName" 'TABLE)

  (test-token "Foo" 'IDENTIFIER "Foo")

  (test-token "types" 'TYPES)

  
  #;((test-token "=" 'EQUAL)
  (test-token "(" 'LPAREN)
  (test-token ")" 'RPAREN)

  (test-token "." 'DOT)
  (test-token "~" 'TILDE)
  (test-token "?" 'QMARK)
  (test-token "!=" 'NEQUAL)
  (test-token "Foo" 'VARIABLE "Foo")
  (test-token "foo" 'IDENTIFIER "foo"))

  )
