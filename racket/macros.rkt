#lang racket
; Faustregel: Funktion/Prozeduren abstrahieren über Werte
;             Makros abstrahieren über Syntax

; Listen destrukturieren
; 2 Probleme:
; - nur 4
; - list-expression wird mehrmals ausgewertet

#;(define-syntax-rule (destructure list-expression (name1 name2 name3 name4) body) ; Syntax-Pattern
  (let ((name1 (first list-expression))
        (name2 (second list-expression))
        (name3 (third list-expression))
        (name4 (fourth list-expression)))
    body))

#;(define-syntax-rule (destructure list-expression (name1 name2 name3 name4) body) ; Syntax-Pattern
  (let ((v list-expression)) ; Hygiene: sorgt für lexikalische Bindung
    (let ((name1 (first v))
          (name2 (second v))
          (name3 (third v))
          (name4 (fourth v)))
      body)))

#;(define-syntax-rule (destructure list-expression (name1 name2 name3 name4) body) ; Syntax-Pattern
  (let ((v list-expression)) ; Hygiene: sorgt für lexikalische Bindung
    (apply (lambda (name1 name2 name3 name4)
             body)
           v)))

; "beliebig viele Namen": name ... müssen immer zusammen vorkommen
#;(define-syntax-rule (destructure list-expression (name ...) body) ; Syntax-Pattern 
  (let ((v list-expression)) ; Hygiene: sorgt für lexikalische Bindung
    (apply (lambda (name ...)
             body)
           v)))

(define-syntax-rule (destructure list-expression (name ...) body) ; Syntax-Pattern 
  (let ((v list-expression))
    (destructure* v (name ...) body)))

(define-syntax destructure*
  (syntax-rules ()  ; "wie match"
    ((destructure* v () body) body)
    ((destructure* list (name0 name ...) body)
     (let ((name0 (first list))
           (v-rest (rest list))) ; rest liefert zweite Hälfte vom Cons, Rest-Liste
       (destructure* v-rest (name ...) body)))))
       
                   
    
    
    

#;(apply (lambda (a b c d) (+ a b c d))
       (list    1 2 3 4))


(define (list-sum l)
  (apply + l)) ; wende Prozedur auf Elemente der Liste an


(define sum
  (lambda numbers ; keine Klammern um numbers!, Argumente werden zu einer Liste
    (list-sum numbers)))
         

; (destructure list-expression (name1 name2 name3 name4) body)
; (destructure (list 1 2 3 4)  (a     b     c     d)     (+ a b c d))

(module+ test
  (require rackunit)
  (check-equal?
   (destructure (list 1 2 3 4)
                (a b c d) ; neue Namen werden gebunden
                (+ a b c d))
   10)
  (check-equal?
   (destructure (list 1 2 3 4)
                (a b c d) ; neue Namen werden gebunden
                (list d a c b))
   (list 4 1 3 2))

  (check-equal?
   (let ((v 17))
     (destructure (list 1 2 3 4)
                  (a b c d)
                  (+ a b c d v)))
   27))

; (if <Bedingung> <Konsequente/"then"> <Alternative/"else">)

(define-syntax if*
  (syntax-rules ()
    ((if* condition then consequent else alternative)
     (if condition consequent alternative))))


(module+ test
  (check-equal?
   (if* #t then 1 else 2)
   1)
  (check-equal?
   (if* #f then 1 else 2)
   2))

