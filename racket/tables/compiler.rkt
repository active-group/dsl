#lang racket/base
(require racket/match
         racket/contract
         syntax/strip-context
         "ast.rkt"
         "parser.rkt")
(provide compile-program compile-table-definition)

(define t3 (parse-from-string "DEFINE TABLE t3 WITH StructName (STR segment \"Segment\", STR country \"Country\")"))

#;(define t3 (table-definition "t3" "StructName"
                            (list
                             (field-definition 'STR "segment" "Segment")
                             (field-definition 'STR "country" "Country")
                             )))

#;(define-table t3 StructName
    (segment "Segment" string)
    (country "Country" string))

(define (compile-table-definition def)
  (match def
    ((table-definition name struct-name field-definitions)
     #`(define-table #,(string->symbol name) #,(string->symbol struct-name)
         #,@(map (lambda (def) ; @ entfernt einen Level Klammern
                   (match def
                     ((field-definition type canon-name title)
                      (let ((type
                             (match type
                               ('STR 'string)
                               ('INT 'int)
                               (`CURRENCY 'currency))))
                        #`(#,(string->symbol canon-name) #,title #,type)))))
                 field-definitions)))))
         
(define (compile-program table-definition)
  (list (strip-context (compile-table-definition table-definition))
        (strip-context
         #`(provide #,(string->symbol (table-definition-name table-definition))))
        (strip-context
         #`(provide (struct-out #,(string->symbol (table-definition-struct-name table-definition)))))))
