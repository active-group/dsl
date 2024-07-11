#lang racket/base
(require parser-tools/lex
         parser-tools/yacc
         racket/list
         racket/contract
         "lex.rkt"
         "ast.rkt")

(define current-source-name (make-parameter #f))

(define (make-srcloc start-pos end-pos)
  (list (current-source-name) 
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos))))

(define-values
  (program-parser column-parser)
  (apply
   values
   (parser
    (start program column)
    (end EOF)
    (tokens dtokens dpunct)
    (src-pos)
    (error
     (lambda (tok-ok? tok-name tok-value start-pos end-pos)
       (raise-syntax-error 'datalog
                           (if tok-ok?
                               (format "Unexpected token ~S" tok-name)
                               (format "Invalid token ~S" tok-name))
                           (datum->syntax #f tok-value
                                          (make-srcloc start-pos end-pos)))))
    (grammar
     (program [(DATA format record columns) (program $2 $3 $4)])
     (format [(FORMAT IDENTIFIER) $2])
     (record [(RECORD IDENTIFIER) $2])
     (columns [() '()]
              [(column columns) (cons $1 $2)])
     (column [(COLUMN LPAREN IDENTIFIER
                      COMMA IDENTIFIER
                      COMMA STRING RPAREN) (column $3 $5 $7)]))

    (suppress))))

(define (parse parser input-port)
  (parameterize ([current-source-name (object-name input-port)]
                 [file-path (object-name input-port)])
    (port-count-lines! input-port)
    (parser (lambda () (dlexer input-port)))))