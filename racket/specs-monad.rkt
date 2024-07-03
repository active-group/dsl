#lang racket
; Brauchen in unserer Monade, zusätzlich zu unit und bind:
; - Befehle
; - Exception-Handling
; - Fail
; Eine Computation ist eins der folgenden:
(struct unit (result) #:transparent)
(struct commandm
  (name
   arg
   continuation) ; Funktion, die gibt eine Computation zurück
  #:transparent)
(struct failm (continuation) #:transparent)
(struct handlem
  (computation ; "try-Block"
   handler-computation
   continuation) ; "catch-Block"
  #:transparent)

(define (bind comp next)
  (match comp
    ((unit result) (next result))
    ((commandm name arg continuation)
     (commandm name arg
               (lambda (result)
                 (bind (continuation result) next))))
    ((failm continuation)
     (failm (lambda (result)
              (bind (continuation result) next))))
    ((handlem computation handler-computation continuation)
     (handlem computation handler-computation
              (lambda (result)
                (bind (continuation result) next))))))

(define (command name arg)
  (commandm name arg unit))
(define (fail)
  (failm unit))

(define-syntax begin_
  (syntax-rules ()
    ((begin_) (unit 'empty-begin))
    ((begin_ ?first ?rest ...)
     (bind ?first
           (lambda (is-mir-doch-egal)
             (begin_ ?rest ...))))))

(define-syntax let_
  (syntax-rules (handle)
    ((let_ ?bindings ?body (handle ?handler))
     (let ?bindings (handlem ?body ?handler unit)))))

(define-syntax while
  (syntax-rules (from)
    ((while ?var (from) ?body)
     (begin_))
    ((while ?var (from ?exp1 ?exp2 ...) ?body)
     (begin_
       (let_ ((?var ?exp1))
             ?body
             (handle (begin_)))
       (while ?var (from ?exp2 ...) ?body)))))
  
(define p1
  (let_ ((a (+ 1 2)))
        (while x (from 1 (+ 1 1) a)
               (begin_ (command "baz" x)
                       (while y (from 4 5 6)
                              (let_ ()
                                    (command "foo" (+ x y))
                                    (handle
                                     (command "bar" 1))))))
        (handle
         (command "baz" 2))))
