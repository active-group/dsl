#lang racket/base
(require racket/match
         racket/contract
         syntax/strip-context
         (prefix-in ast: "ast.rkt")
         "tim.rkt")

(provide
 (contract-out
  (compile-program (ast:program? . -> . (listof syntax?)))
  (compile-declaration (ast:declaration/c . -> . syntax?))))

(define compile-expression
  (match-lambda
    ((ast:record srcloc name direction field-formats)
     (quasisyntax/loc (loc-syntax srcloc)
       (record-format #,(string->symbol name) #,(compile-expression direction)
                      (list #,@(map compile-expression field-formats)))))
    ((ast:list srcloc direction element-format)
     (quasisyntax/loc (loc-syntax srcloc)
       (list-format #,(compile-expression direction)
                    #,(compile-expression element-format))))
     
    ((ast:ignore srcloc direction field-formats)
     (quasisyntax/loc (loc-syntax srcloc)
       (ignore #,(compile-expression direction)
               (list #,@(map compile-expression field-formats)))))
    ((ast:choose srcloc index direction formats)
     (quasisyntax/loc (loc-syntax srcloc)
       (choose #,index #,(compile-expression direction)
               (list #,@(map compile-expression formats)))))
    ((ast:direction srcloc right down)
     (quasisyntax/loc (loc-syntax srcloc)
       (direction #,right #,down)))     
    ((ast:coordinates srcloc x y)
     (quasisyntax/loc (loc-syntax srcloc)
       (position #,x #,y)))
    ((ast:reference srcloc name)
     (quasisyntax/loc (loc-syntax srcloc)
       #,(string->symbol name)))
    ((ast:constant srcloc text)
     (quasisyntax/loc (loc-syntax srcloc)
       (constant-format #,text)))))

(define compile-declaration
  (match-lambda
    ((ast:defrecord srcloc name fields)
     (quasisyntax/loc (loc-syntax srcloc)
       (struct #,(string->symbol name)
         #,(map string->symbol fields))))
    ((ast:equation srcloc name rhs)
     (quasisyntax/loc (loc-syntax srcloc)
       (define #,(string->symbol name)
         #,(compile-expression rhs))))))

(define compile-program
  (match-lambda
    ((ast:program srcloc declarations format-name coordinates)
     (append (map strip-context
                  (map compile-declaration declarations))
             (list
              (strip-context
               (quasisyntax
                (define (parse table)
                  (parse-table #,(string->symbol format-name)
                               table
                               #,(compile-expression coordinates)))))
              (strip-context
               #`(provide parse #,(string->symbol format-name))))))))

(define (loc-syntax srcloc)
  (datum->syntax #f 'x srcloc))

(module+ test
  (require rackunit)

  (define src1 (srcloc 1 2 3 4 5))
  (define src2 (srcloc 2 3 4 5 6))
  (define src3 (srcloc 3 4 5 6 7))
  (define src4 (srcloc 4 5 6 7 8))

  (check-equal?
   (syntax->datum
    (compile-expression
     (ast:constant src1 "foo")))
   '(constant-format "foo"))

  (check-equal?
   (syntax->datum
    (compile-expression
     (ast:coordinates src1 42 43)))
   '(position 42 43))

  (check-equal?
   (syntax->datum
    (compile-expression
     (ast:direction src1 42 43)))
   '(direction 42 43))

  (check-equal?
   (syntax->datum
    (compile-expression
     (ast:reference src1 "foo")))
   'foo)

  (check-equal?
   (syntax->datum
    (compile-expression
     (ast:choose src1 2 (ast:reference src2 "down") (list (ast:reference src3 "heading") (ast:reference src4 "entries")))))
   '(choose 2 down (list heading entries)))

  (check-equal?
   (syntax->datum
    (compile-expression
     (ast:ignore src1 (ast:reference src2 "down") (list (ast:reference src3 "heading") (ast:reference src4 "entries")))))
   '(ignore down (list heading entries)))

  (check-equal?
   (syntax->datum
    (compile-expression
     (ast:record src1 "foo" (ast:reference src2 "down") (list (ast:reference src3 "heading") (ast:reference src4 "entries")))))
   '(record-format foo down (list heading entries)))
  
  (check-equal?
   (syntax->datum
    (compile-expression
     (ast:list src1 (ast:reference src2 "down") (ast:reference src3 "element"))))
   '(list-format down element))

  (check-equal?
   (syntax->datum
    (compile-declaration
     (ast:defrecord src1 "pare" (list "kar" "kdr"))))
   '(struct pare (kar kdr)))

  (check-equal?
   (syntax->datum
    (compile-declaration
     (ast:equation src1 "right"  (ast:direction src2 1 0))))
   '(define right (direction 1 0)))
  
     
  )
    
