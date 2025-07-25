#lang racket/base
(require parser-tools/lex
         parser-tools/yacc
         racket/list
         racket/contract
         "lexer.rkt"
         "ast.rkt")

(define current-source-name (make-parameter #f))

(define (make-srcloc start-pos end-pos)
  (srcloc (current-source-name) 
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos))))

(define-values (table-definition-parser)
  (apply
   values
   (parser
    (start table-definition)
    (end EOF)
    (tokens tokens-with-payload plain-tokens)
    (src-pos)
    (error
     (lambda (ok? name value start-pos end-pos)
       (raise-syntax-error 'tables
                           (if ok?
                               (format "Unexpected token ~S" name)
                               (format "Invalid token ~S" name))
                           (datum->syntax #f value (make-srcloc start-pos end-pos)))))
    (grammar
     ...))))
