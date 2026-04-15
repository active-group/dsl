#lang racket/base
(require parser-tools/lex
         parser-tools/yacc
         racket/list
         racket/contract
         "lexer.rkt"
         (prefix-in ast: "ast.rkt"))

; BNF unserer Sprache
; set-assignment -> IDENTIFIER ASSIGN set-definition
; set-definition -> LEFT_BRACE set-definition-body RIGHT_BRACE
; set-definition-body -> list-of-numbers
;                     |  element-definition WITH list-set-constraints
; list-of-numbers -> NUMBER | NUMBER COMMA list-of-numbers
; element-definition -> IDENTIFIER IN INDENTIFIER
; list-set-constraints -> set-constraint
;                      |  set-constraint COMMA list-set-constraints
; set-constraint -> table-lookup EQUAL value
; table-lookup -> IDENTIFIER LEFT-PAREN IDENTIFIER RIGHT-PAREN
; value -> NUMBER | DPPLUS | DPMINUS

#;(provide
   (contract-out
    (current-source-name (parameter/c any/c))
    (parse-declaration (input-port? . -> . ast:declaration/c))
    (parse-program (input-port? . -> . ast:program?))))

(define current-source-name (make-parameter #f))

(define (make-srcloc start-pos end-pos)
  (srcloc (current-source-name) 
          (position-line start-pos)
          (position-col start-pos)
          (position-offset start-pos)
          (- (position-offset end-pos) (position-offset start-pos))))

(define-values (program-parser #;declaration-parser #;expression-parser)
  
  (parser
   (start program #;declaration #;expression)
   (end EOF)
   (tokens tokens-with-payload plain-tokens)
   (src-pos)
   (error
    (lambda (ok? name value start-pos end-pos)
      (raise-syntax-error 'set-definition
                          (if ok?
                              (format "Unexpected token ~S" name)
                              (format "Invalid token ~S" name))
                          (datum->syntax #f value (make-srcloc start-pos end-pos)))))
   (grammar
     
    ; program -> set-assignment | EOF
    (program
     #;((declarations TABLE IDENTIFIER coordinates) (ast:program (make-srcloc $2-start-pos $4-end-pos)
                                                                 $1 $3 $4))
     (() #f)
     ((set-assignment) $1))

    ; set-assignment -> IDENTIFIER ASSIGN set-definition
    (set-assignment
     ((IDENTIFIER ASSIGN set-definition)
      (ast:set-assignment (make-srcloc $1-start-pos $3-end-pos)
                          $1
                          $3)))

    ; set-definition -> LEFT_BRACE set-definition-body RIGHT_BRACE(set-definition
    (set-definition
     ((LEFT-BRACE set-definition-body RIGHT-BRACE) $2))

    ; set-definition-body -> list-of-numbers
    ;                     |  element-definition WITH list-set-constraints
    (set-definition-body
     ((list-of-numbers) $1)
     ((element-definition WITH list-set-constraints)
      (ast:set-constraint-with-list-set-constraints
       (make-srcloc $1-start-pos $3-end-pos)
       $1
       $3)))
     
    ; list-of-numbers -> NUMBER | NUMBER COMMA list-of-numbers
    (list-of-numbers
     ((NUMBER) (list $1))
     ((NUMBER COMMA list-of-numbers) (cons $1 $3)))

    ; element-definition -> IDENTIFIER IN INDENTIFIER
    (element-definition
     ((IDENTIFIER IN IDENTIFIER)
      (ast:element-definition
       (make-srcloc $1-start-pos $3-end-pos)
       $1
       $3)))
    
    ; list-set-constraints -> set-constraint
    ;                      |  set-constraint COMMA list-set-constraints
    (list-set-constraints
     ((set-constraint) (list $1))
     ((set-constraint COMMA list-set-constraints) (cons $1 $3)))
    
    ; set-constraint -> table-lookup EQUAL value
    (set-constraint
     ((table-lookup EQUAL value)
      (ast:set-constraint
       (make-srcloc $1-start-pos $3-end-pos)
       $1
       $3)))
    
    ; table-lookup -> IDENTIFIER LEFT-PAREN IDENTIFIER RIGHT-PAREN
    (table-lookup
     ((IDENTIFIER LEFT-PAREN IDENTIFIER RIGHT-PAREN)
      (ast:table-lookup
       (make-srcloc $1-start-pos $4-end-pos)
       $1
       $3)))
    
    ; value -> NUMBER | DPPLUS | DPMINUS
    (value
     ((NUMBER) $1)
     ((DPPLUS) 'dP+)
     ((DPMINUS) 'dP-)))))

(define (parse parser input-port)
  (parameterize ([current-source-name (object-name input-port)]
                 [file-path (object-name input-port)])
    (port-count-lines! input-port)
    (parser (lambda () (token-lexer input-port)))))

(module+ test
  (require rackunit)
  (define (test-program input-string expected-program)
    (test-equal? input-string
                 (parse program-parser (open-input-string input-string))
                 expected-program))

  (test-program "A := { 1 }"
                (ast:set-assignment (srcloc 'string 1 0 1 10) "A" '(1)))
  (test-program "A := { 1,2,   3 }"
                (ast:set-assignment (srcloc 'string 1 0 1 17) "A" '(1 2 3)))
  (test-program "B := { a in A | DOF(a) = dP+}"
                (ast:set-assignment
                 (srcloc 'string 1 0 1 29)
                 "B"
                 (ast:set-constraint-with-list-set-constraints
                  (srcloc 'string 1 7 8 21)
                  (ast:element-definition (srcloc 'string 1 7 8 6) "a" "A")
                  (list
                   (ast:set-constraint
                    (srcloc 'string 1 16 17 12)
                    (ast:table-lookup (srcloc 'string 1 16 17 6) "DOF" "a")
                    'dP+)))))
  (test-program "B := { a in A | DOF(a) = dP-}"
                (ast:set-assignment
                 (srcloc 'string 1 0 1 29)
                 "B"
                 (ast:set-constraint-with-list-set-constraints
                  (srcloc 'string 1 7 8 21)
                  (ast:element-definition (srcloc 'string 1 7 8 6) "a" "A")
                  (list
                   (ast:set-constraint
                    (srcloc 'string 1 16 17 12)
                    (ast:table-lookup (srcloc 'string 1 16 17 6) "DOF" "a")
                    'dP-)))))
  (test-program "B := { a in A | DOF(a) = dP-, Group(a) = 2}"
                (ast:set-assignment
                 (srcloc 'string 1 0 1 43)
                 "B"
                 (ast:set-constraint-with-list-set-constraints
                  (srcloc 'string 1 7 8 35)
                  (ast:element-definition (srcloc 'string 1 7 8 6) "a" "A")
                  (list
                   (ast:set-constraint
                    (srcloc 'string 1 16 17 12)
                    (ast:table-lookup (srcloc 'string 1 16 17 6) "DOF" "a")
                    'dP-)
                   (ast:set-constraint
                    (srcloc 'string 1 30 31 12)
                    (ast:table-lookup (srcloc 'string 1 30 31 8) "Group" "a")
                    2))))))