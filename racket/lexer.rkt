#lang racket/base
(provide token-lexer
         tokens-with-payload
         plain-tokens)
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens tokens-with-payload (IDENTIFIER STRING NUMBER))
(define-empty-tokens plain-tokens
  (LEFT-PAREN COMMA RIGHT-PAREN LEFT-BRACE RIGHT-BRACE
   SEMICOLON EQUALS HASH AT
   DEFRECORD
   RECORD LIST IGNORE CHOOSE TABLE
   EOF))

(define-lex-abbrev line-break #\newline)
(define-lex-abbrev identifier-characters (char-complement (char-set "(;,)=:.~?\"% \n")))
(define-lex-abbrev identifier-regexp (:: identifier-characters (:* (:or upper-case identifier-characters))))
(define-lex-abbrev number-regexp (:+ numeric))
(define-lex-abbrev comment-regexp (:: "//" (complement (:: any-string line-break any-string)) line-break))

; parse string literal after opening quote
(define string-lexer
  (lexer
   ((eof) (error 'tim-lexer "Unterminated string"))
   ((:~ #\" #\\ #\newline) (cons (car (string->list lexeme))
                                 (string-lexer input-port)))
   ((:: #\\ #\\) (cons #\\ (string-lexer input-port)))
   ((:: #\\ #\newline) (cons #\newline (string-lexer input-port)))
   ((:: #\\ #\") (cons #\" (string-lexer input-port)))
   (#\" '())))

(define (get-string input-port)
  (list->string (string-lexer input-port)))

(define token-lexer
  (lexer-src-pos
   (whitespace
    (return-without-pos (token-lexer input-port)))
   (comment-regexp
    (return-without-pos (token-lexer input-port)))
   (number-regexp
    (token-NUMBER (string->number lexeme)))
   (#\" (token-STRING (get-string input-port)))
   (#\( (token-LEFT-PAREN))
   (#\) (token-RIGHT-PAREN))
   (#\, (token-COMMA))
   (#\{ (token-LEFT-BRACE))
   (#\} (token-RIGHT-BRACE))
   (#\; (token-SEMICOLON))
   (#\= (token-EQUALS))
   (#\# (token-HASH))
   (#\@ (token-AT))
   ("defrecord" (token-DEFRECORD))
   ("record" (token-RECORD))
   ("list" (token-LIST))
   ("ignore" (token-IGNORE))
   ("choose" (token-CHOOSE))
   ("table" (token-TABLE))
   (identifier-regexp (token-IDENTIFIER lexeme))
   ((eof) (token-EOF))))


(module+ test
  (require rackunit)

  (define (test-token input-string name [value #f])
    (let* ((with-position (token-lexer (open-input-string input-string)))
           (token (position-token-token with-position)))
      (test-equal? (format "lexer: ~a: <~a,~a>" input-string name value)
                   (cons (token-name token) (token-value token))
                   (cons name value))))

  (test-token "=" 'EQUALS)
  (test-token "(" 'LEFT-PAREN)
  (test-token ")" 'RIGHT-PAREN)
  (test-token "," 'COMMA)
  (test-token "#" 'HASH)
  (test-token "@" 'AT)
  (test-token ";" 'SEMICOLON)
  (test-token "{" 'LEFT-BRACE)
  (test-token "}" 'RIGHT-BRACE)
  (test-token "defrecord" 'DEFRECORD)
  (test-token "record" 'RECORD)
  (test-token "list" 'LIST)
  (test-token "ignore" 'IGNORE)
  (test-token "choose" 'CHOOSE)
  (test-token "table" 'TABLE)

  (test-token "    =" 'EQUALS)

  (test-token "    foo;" 'IDENTIFIER "foo")

  (test-token "val_" 'IDENTIFIER "val_")
  (test-token "912Kadf" 'IDENTIFIER "912Kadf")
  (test-token "\"foo\"" 'STRING "foo")
  (test-token "\"\\\"\"" 'STRING "\"")

  (test-token "123" 'NUMBER 123)

  (test-token "// 12453\n=" 'EQUALS)

  )
