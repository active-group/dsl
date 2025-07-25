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

#|
  DEFINE TABLE t3 WITH StructName (STR segment "Segment", STR country "Country")

  (get-tokens-from-string "DEFINE TABLE t3 WITH StructName (STR segment \"Segment\", STR country \"Country\")")
  |#

(define table-definition-parser
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
    (table-definition ((DEFINE TABLE IDENTIFIER WITH IDENTIFIER LPAREN field-definitions RPAREN)  (table-definition $3 $5 $7)))
    (field-definitions (() '())
                       ((field-definition) (list $1))
                       ((field-definition COMMA field-definitions) (cons $1 $3)))
    (field-definition ((type IDENTIFIER STRING) (field-definition $1 $2 $3)))
    (type ((STR) 'STR)
          ((INT) 'INT)
          ((CURRENCY) 'CURRENCY)))))


(define (parse input-port)
  (parameterize ([current-source-name (object-name input-port)]
                 [file-path (object-name input-port)])
    (port-count-lines! input-port)
    (table-definition-parser (lambda () (dlexer input-port)))))

(define (parse-from-string string)
  (parse (open-input-string string)))

(define (parse-from-file filename)
  (call-with-input-file filename parse))
