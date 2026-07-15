#lang racket/base

(define table1
  '(("Segment" "Country" "Units Sold" "Manuf. Price" "Sale Price")
    ("Government" "Canada" 1618 3 20)
    ))

(struct entry
  (segment
   country
   units-sold
   manufacturing-price
   sale-price)
  #:transparent)

(define (table-ref table row column)
  (list-ref (list-ref table row) column))

; Strukturierte Daten: Liste von entry-Structs

(struct cell
  (datatype)
  #:transparent)

; record-type is an cell or a list of cell
; sheet is a record or list of records
; all of those we can also call area

(struct record-type
  (table-formats ; list of
   direction)
  #:transparent)

(struct list-type
  (table-format
   direction)
  #:transparent)

(struct header
  (label)
  #:transparent)


; KOMBINATORTYPE !!!! table format is either cell , record-type or header

(define headers1 (record-type ( list (header "Segment") (header "Country") (header "Units Sold") (header "Manuf. Price") (header "Sale Price")) 'horizontal))
(define record-type1 (record-type ( list (cell 'string)(cell 'string)(cell 'number)(cell 'number)(cell 'number)) 'horizontal))
(define entries1 (list-type record-type1 'vertical))


(define format1 (record-type (list headers1 entries1) 'vertical))



(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens dtokens (LABEL))
(define-empty-tokens dpunct (LPAREN RPAREN RECORD STRING NUMBER EOF))
(define-lex-abbrev line-break #\newline)
(define-lex-abbrev id-chars (char-complement (char-set "(,)=:.~?\"% \n")))
(define-lex-abbrev record-re "Record")
(define-lex-abbrev number-re "Number")
(define-lex-abbrev string-re "String")
(define-lex-abbrev comment-re (:: "&" (complement (:: any-string line-break any-string)) line-break))

(define get-string-token
  (lexer
   [(eof) (error 'datalog-lexer "Unterminated string")]
   [(:~ #\" #\\ #\newline) (cons (car (string->list lexeme))
                                 (get-string-token input-port))]
   [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
   [(:: #\\ #\newline) (cons #\newline (get-string-token input-port))]
   [(:: #\\ #\") (cons #\" (get-string-token input-port))]
   [#\" null]))

(define dlexer
  (lexer-src-pos
   [whitespace
    (return-without-pos (dlexer input-port))]
   [comment-re
    (return-without-pos (dlexer input-port))]
   [record-re
    (token-RECORD)]
   [string-re
    (token-STRING)]
   [number-re
    (token-NUMBER)]
   [#\" (token-LABEL (list->string (get-string-token input-port)))]
   [#\( (token-LPAREN)]
   [#\) (token-RPAREN)]
   [(eof) (token-EOF)]))

(provide dtokens dpunct
         line-break record-re comment-re
         dlexer)

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
  
  (test-token "(" 'LPAREN)
  (test-token ")" 'RPAREN)
  (test-token "Record" 'RECORD)
  (test-token "Number" 'NUMBER)
  (test-token "String" 'STRING)
  (test-token "\"Segment\"" 'LABEL "Segment")
  

  (get-tokens-from-file "/home/serafin/Documents/Code/dsl/serafin_roman/format.tafo")
  )