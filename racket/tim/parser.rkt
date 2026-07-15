#lang racket/base
(require parser-tools/lex
         parser-tools/yacc
         racket/list
         racket/contract
         "lexer.rkt"
         (prefix-in ast: "ast.rkt"))

(provide
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

(define-values (program-parser declaration-parser expression-parser)
  (apply
   values
   (parser
    (start program declaration expression)
    (end EOF)
    (tokens tokens-with-payload plain-tokens)
    (src-pos)
    (error
     (lambda (ok? name value start-pos end-pos)
       (raise-syntax-error 'tim
                           (if ok?
                               (format "Unexpected token ~S" name)
                               (format "Invalid token ~S" name))
                           (datum->syntax #f value (make-srcloc start-pos end-pos)))))
    (grammar
     (program
      ((declarations TABLE IDENTIFIER coordinates) (ast:program (make-srcloc $2-start-pos $4-end-pos)
                                                                $1 $3 $4)))
     (declarations (() '())
                   ((declaration SEMICOLON declarations) (cons $1 $3)))
     (declaration ((equation) $1)
                  ((record-definition) $1))
     (equation ((IDENTIFIER EQUALS expression)
                (ast:equation (make-srcloc $1-start-pos $3-end-pos)
                              $1 $3)))
     (record-definition ((DEFRECORD IDENTIFIER LEFT-BRACE identifiers RIGHT-BRACE)
                         (ast:defrecord (make-srcloc $1-start-pos $5-end-pos)
                                        $2 $4)))
     (identifiers (() '())
                  ((IDENTIFIER) (list $1))
                  ((IDENTIFIER COMMA identifiers) (cons $1 $3)))

     (expression ((direction) $1)
                 ((coordinates) $1)
                 ((record) $1)
                 ((list) $1)
                 ((ignore) $1)
                 ((choose) $1)
                 ((STRING) (ast:constant (make-srcloc $1-start-pos $1-end-pos) $1))
                 ((IDENTIFIER) (ast:reference (make-srcloc $1-start-pos $1-end-pos) $1)))

     (expressions (() '())
                  ((expression) (list $1))
                  ((expression COMMA expressions) (cons $1 $3)))
     
     (direction ((HASH LEFT-PAREN NUMBER COMMA NUMBER RIGHT-PAREN)
                 (ast:direction (make-srcloc $1-start-pos $6-end-pos)
                                $3 $5)))
     (coordinates ((AT LEFT-PAREN NUMBER COMMA NUMBER RIGHT-PAREN)
                   (ast:coordinates (make-srcloc $1-start-pos $6-end-pos)
                                    $3 $5)))

     (record ((RECORD IDENTIFIER LEFT-PAREN expression RIGHT-PAREN LEFT-BRACE expressions RIGHT-BRACE)
              (ast:record (make-srcloc $1-start-pos $8-end-pos)
                          $2 $4 $7)))
     (list ((LIST LEFT-PAREN expression RIGHT-PAREN expression)
            (ast:list (make-srcloc $1-start-pos $5-end-pos)
                      $3 $5)))
     (ignore ((IGNORE LEFT-PAREN expression RIGHT-PAREN LEFT-BRACE expressions RIGHT-BRACE)
              (ast:ignore (make-srcloc $1-start-pos $7-end-pos)
                          $3 $6)))

     (choose ((CHOOSE NUMBER LEFT-PAREN expression RIGHT-PAREN LEFT-BRACE expressions RIGHT-BRACE)
              (ast:choose (make-srcloc $1-start-pos $8-end-pos)
                          $2 $4 $7)))))))

(define (parse parser input-port)
  (parameterize ([current-source-name (object-name input-port)]
                 [file-path (object-name input-port)])
    (port-count-lines! input-port)
    (parser (lambda () (token-lexer input-port)))))

(define (parse-program input-port)
  (parse program-parser input-port))

(define (parse-declaration input-port)
  (parse declaration-parser input-port))

(module+ test
  (require rackunit)

  (define (test-expression input-string expected-expression)
    (test-equal? input-string
                 (parse expression-parser (open-input-string input-string))
                 expected-expression))

  (define (test-declaration input-string expected-declaration)
    (test-equal? input-string
                 (parse declaration-parser (open-input-string input-string))
                 expected-declaration))

    (define (test-program input-string expected-program)
      (test-equal? input-string
                   (parse program-parser (open-input-string input-string))
                   expected-program))

  (test-expression "foo"
                   (ast:reference (srcloc 'string 1 0 1 3) "foo"))
  (test-expression "\"foo\""
                   (ast:constant (srcloc 'string 1 0 1 1) "foo"))
  (test-expression "#(1,2)"
                   (ast:direction (srcloc 'string 1 0 1 6) 1 2))
  (test-expression "@(1,2)"
                   (ast:coordinates (srcloc 'string 1 0 1 6) 1 2))
  (test-expression "record entry (right) { text, text, number }"
                   (ast:record (srcloc 'string 1 0 1 43)
                               "entry" (ast:reference (srcloc 'string 1 14 15 5) "right")
                               (list (ast:reference (srcloc 'string 1 23 24 4) "text")
                                     (ast:reference (srcloc 'string 1 29 30 4) "text")
                                     (ast:reference (srcloc 'string 1 35 36 6) "number"))))

  (test-expression "list (down) entry"
                   (ast:list
                    (srcloc 'string 1 0 1 17)
                    (ast:reference (srcloc 'string 1 6 7 4) "down")
                    (ast:reference (srcloc 'string 1 12 13 5) "entry")))
  (test-expression "ignore (right) { \"Name\", \"Address\", \"Phone\" }"
                   (ast:ignore
                    (srcloc 'string 1 0 1 45)
                    (ast:reference (srcloc 'string 1 8 9 5) "right")
                    (list
                     (ast:constant (srcloc 'string 1 17 18 1) "Name")
                     (ast:constant (srcloc 'string 1 25 26 1) "Address")
                     (ast:constant (srcloc 'string 1 36 37 1) "Phone"))))
  (test-expression "choose 2 (down) { heading, entries }"
                   (ast:choose
                    (srcloc 'string 1 0 1 36)
                    2
                    (ast:reference (srcloc 'string 1 10 11 4) "down")
                    (list
                     (ast:reference (srcloc 'string 1 18 19 7) "heading")
                     (ast:reference (srcloc 'string 1 27 28 7) "entries"))))

  #;(test-declaration "defrecord entry { name, address, phone }"
                    (ast:defrecord (srcloc 'string 1 0 1 40) "entry" (srcloc '"name" "address" "phone")))
  (test-declaration "right = #(1, 0)"
                    (ast:equation (srcloc 'string 1 0 1 15) "right" (ast:direction (srcloc 'string 1 8 9 7) 1 0)))
  (test-declaration "entry = record entry (right) { text, text, number }"
                      (ast:equation
                       (srcloc 'string 1 0 1 51)
                       "entry"
                       (ast:record
                        (srcloc 'string 1 8 9 43)
                        "entry"
                        (ast:reference (srcloc 'string 1 22 23 5) "right")
                        (list
                         (ast:reference (srcloc 'string 1 31 32 4) "text")
                         (ast:reference (srcloc 'string 1 37 38 4) "text")
                         (ast:reference (srcloc 'string 1 43 44 6) "number")))))

  #;(test-program "// address entry
defrecord entry { name, address, phone };

right = #(1, 0);
down = #(0, 1);

entry = record entry (right) { text, text, number };
entries = list (down) entry;
heading = ignore (right) { \"Name\", \"Address\", \"Phone\" };
table = choose 2 (down) { heading, entries };

table @(0, 0)"

                  #f)

  (test-program "table foo @(0,0)"
                (ast:program (srcloc 'string 1 0 1 16) '() "foo" (ast:coordinates (srcloc 'string 1 10 11 6) 0 0)))

  (test-program "right = #(1, 0);
table foo @(0,0)"
                (ast:program
                 (srcloc 'string 2 0 18 16)
                 (list (ast:equation (srcloc 'string 1 0 1 15) "right" (ast:direction (srcloc 'string 1 8 9 7) 1 0)))
                 "foo"
                 (ast:coordinates (srcloc 'string 2 10 28 6) 0 0)))

    (test-program "right = #(1, 0);
table foo @(0,0)"
                (ast:program
                 (srcloc 'string 2 0 18 16)
                 (list (ast:equation (srcloc 'string 1 0 1 15) "right" (ast:direction (srcloc 'string 1 8 9 7) 1 0)))
                 "foo"
                 (ast:coordinates (srcloc 'string 2 10 28 6) 0 0)))


    (test-program "// address entry
defrecord entry { name, address, phone };

right = #(1, 0);
down = #(0, 1);

entry = record entry (right) { text, text, number };
entries = list (down) entry;
heading = ignore (right) { \"Name\", \"Address\", \"Phone\" };
addressbook = choose 2 (down) { heading, entries };

table addressbook @(0, 0)"
                  (ast:program
                   (srcloc 'string 12 0 287 25)
                   (list
                    (ast:defrecord (srcloc 'string 2 0 18 40) "entry" '("name" "address" "phone"))
                    (ast:equation (srcloc 'string 4 0 61 15) "right" (ast:direction (srcloc 'string 4 8 69 7) 1 0))
                    (ast:equation (srcloc 'string 5 0 78 14) "down" (ast:direction (srcloc 'string 5 7 85 7) 0 1))
                    (ast:equation
                     (srcloc 'string 7 0 95 51)
                     "entry"
                     (ast:record
                      (srcloc 'string 7 8 103 43)
                      "entry"
                      (ast:reference (srcloc 'string 7 22 117 5) "right")
                      (list
                       (ast:reference (srcloc 'string 7 31 126 4) "text")
                       (ast:reference (srcloc 'string 7 37 132 4) "text")
                       (ast:reference (srcloc 'string 7 43 138 6) "number"))))
                    (ast:equation
                     (srcloc 'string 8 0 148 27)
                     "entries"
                     (ast:list
                      (srcloc 'string 8 10 158 17)
                      (ast:reference (srcloc 'string 8 16 164 4) "down")
                      (ast:reference (srcloc 'string 8 22 170 5) "entry")))
                    (ast:equation
                     (srcloc 'string 9 0 177 55)
                     "heading"
                     (ast:ignore
                      (srcloc 'string 9 10 187 45)
                      (ast:reference (srcloc 'string 9 18 195 5) "right")
                      (list
                       (ast:constant (srcloc 'string 9 27 204 1) "Name")
                       (ast:constant (srcloc 'string 9 35 212 1) "Address")
                       (ast:constant (srcloc 'string 9 46 223 1) "Phone"))))
                    (ast:equation
                     (srcloc 'string 10 0 234 50)
                     "addressbook"
                     (ast:choose
                      (srcloc 'string 10 14 248 36)
                      2
                      (ast:reference (srcloc 'string 10 24 258 4) "down")
                      (list
                       (ast:reference (srcloc 'string 10 32 266 7) "heading")
                       (ast:reference (srcloc 'string 10 41 275 7) "entries")))))
                   "addressbook"
                   (ast:coordinates (srcloc 'string 12 18 305 7) 0 0)))
     )
