#lang racket/base

(require parser-tools/lex
         parser-tools/yacc
         racket/list
         racket/contract
         "linksLexer.rkt"
         "ast.rkt")



(define current-source-name (make-parameter #f))

(define (make-srcloc start-pos end-pos)
  (list (current-source-name) 
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos))))



(define g
  (parser
   (start table)
   (end EOF)
   (tokens dtokens dpunct)
   (src-pos)
   (error
    (lambda (tok-ok? tok-name tok-value start-pos end-pos)
      (raise-syntax-error 'datalog
                          (if tok-ok?
                              (format "Unexpected token ~S (~S)" tok-name tok-value)
                              (format "Invalid token ~S" tok-name))
                          (datum->syntax #f tok-value (make-srcloc start-pos end-pos)))))
   (grammar
    (table [(TABLE COLON IDENTIFIER headers types fields) (table (make-srcloc $1-start-pos (node-srcloc (last $6))) $3 $4 $5 $6)])
    (headers ((HEADER COLON values) $3))
    (types ((TYPES COLON values) $3))
    (fields ((FIELDS COLON values) $3))
    (values [() '()]
            [(value) (cons $1 '())]
            [(value COMMA values) (cons $1 $3 )])
    (value [(NAME) (item (make-srcloc $1-start-pos $1-end-pos) $1)]
           [(IDENTIFIER) (item (make-srcloc $1-start-pos $1-end-pos) $1)]
           )
    )))

(define ((mk-parser which) ip)
  (define (go)
    (port-count-lines! ip)
    (which (lambda () (dlexer ip))))
  (if (current-source-name)
      (go)
      (parameterize ([current-source-name (object-name ip)]
                     [file-path (object-name ip)])
        (go))))  

(define parse-table (mk-parser g))

(module+ test
  (require rackunit)

  (define (test-table-parse str res)
    (check equal? (parse-table (open-input-string str)) res))

  (test-table-parse "Table: TableName
 header : Segment, Country, Units Sold
 types: string, string, number
 fields: segment, country, units-sold"
                     #f)
  
)
