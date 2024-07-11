#lang racket
(require (prefix-in ast: "ast.rkt")
         "table.rkt")
(provide compile-program)

(define (compile-program p)
  (match p
    ((ast:program format record columns)
     (quasisyntax
      (begin
        (provide #,(string->symbol format)
                 #,(string->symbol record))
        (default-data-set #,(string->symbol format)
          #,(string->symbol record)
          #,@(map (lambda (c) ; @: die Elemente der Liste werden eingeführt
                    ; bzw. die äußeren Klammern werden weggemacht
                 (match c
                   ((ast:column field-format field-name heading)
                    (quasisyntax
                     (column #,(string->symbol field-format)
                             #,(string->symbol field-name)
                             #,heading)))))
                  columns)))))))
        