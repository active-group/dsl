#lang racket/base

; Format -> record Pair*
; Pair -> lpar label type rpar

(require parser-tools/lex
         parser-tools/yacc
         racket/list
         racket/contract
         "tafo-lexer.rkt"
         "tafo-ast.rkt")

(define current-source-name (make-parameter #f))

(define (make-srcloc start-pos end-pos)
  (list (current-source-name) 
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos))))

(define-values
  (program-parser pair-parser)
  (apply
   values
   (parser
    (start program pair)
    (end EOF)
    (tokens dtokens dpunct)
    (src-pos)
    (error
     (lambda (tok-ok? tok-name tok-value start-pos end-pos)
       (raise-syntax-error 'tafo
                           (if tok-ok?
                               (format "Unexpected token ~S" tok-name)
                               (format "Invalid token ~S" tok-name))
                           (datum->syntax #f tok-value (make-srcloc start-pos end-pos)))))
    (grammar
     (program ((RECORD pairs) (program (make-srcloc $1-start-pos $2-end-pos) $2)))
     (pairs [(pair) (list $1)]
            [(pair pairs) (cons $1 $2)])
     (pair (( LPAREN LABEL type RPAREN ) (pair  (make-srcloc $1-start-pos $4-end-pos) $2 $3)))
     (type [(NUMBER) 'Number]
           [(STRING) 'String])
     )
    (suppress))))


(define ((mk-parser which) ip)
  (define (go)
    (port-count-lines! ip)
    (which (lambda () (dlexer ip))))
  (if (current-source-name)
      (go)
      (parameterize ([current-source-name (object-name ip)]
                     [file-path (object-name ip)])
        (go))))  

(define parse-program (mk-parser program-parser))
(define parse-pair (mk-parser pair-parser))


(provide
 current-source-name
 parse-program
 parse-pair
 )

(module+ test
  (require rackunit)

  (define (test-pair-parse str res)
    (check equal? (parse-pair (open-input-string str)) res))

  (test-pair-parse "           (   \"Segment\"      Number    )"
                     (pair '(string 1 11 12 30) "Segment" 'Number))


  (define (test-program-parse str res)
    (check equal? (parse-program (open-input-string str)) res))

  (test-program-parse "    Record       (   \"Segment\"      Number    )"
                     (program '(string 1 4 5 43) (list (pair '(string 1 17 18 30) "Segment" 'Number))))

  (define (test-file-parse file res)
    (check equal? (parse-program (open-input-file file)) res))

  (test-file-parse "./format.tafo"
                     #f)
  )

