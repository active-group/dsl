(module reader syntax/module-reader
  #:language 'tim/lang
  #:read (lambda ([in (current-input-port)]) (this-read-syntax #f in))
  #:read-syntax this-read-syntax
  #:whole-body-readers? #t
  #:language-info '#(tables/lang/lang-info get-info #f)
  (require "../parse.rkt"
           "../compiler.rkt"
           syntax/strip-context)
  
  (define (this-read-syntax [src #f] [in (current-input-port)])
    (list
     (strip-context #'(require tables/table))
     (strip-context
      (compile-program
       (parameterize ([current-source-name src])
         (parse-program in)))))))

