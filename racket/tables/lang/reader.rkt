(module reader syntax/module-reader
  #:language 'tables/lang
  #:read (lambda ([in (current-input-port)]) (this-read-syntax #f in))
  #:read-syntax this-read-syntax
  #:whole-body-readers? #t
  #:language-info '#(tables/lang/lang-info get-info #f)
  (require "../parser.rkt"
           "../compiler.rkt")
  
  (define (this-read-syntax [src #f] [in (current-input-port)])
    (compile-program
     (parameterize ([current-source-name src])
       (parse in)))))
