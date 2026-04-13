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

(define-syntax-rule (destructure list-expression (name1 name2 name3 name4) body) ; Syntax-Pattern
  (let ((v list-expression)) ; Hygiene: sorgt für lexikalische Bindung
    (let ((name1 (first v))
          (name2 (second v))
          (name3 (third v))
          (name4 (fourth v)))
      body)))

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
   (list 4 1 3 2)))

