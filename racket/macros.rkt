#lang racket
; Faustregel: Funktion/Prozeduren abstrahieren über Werte
;             Makros abstrahieren über Syntax

; Listen destrukturieren
(define-syntax-rule (destructure list-expression (name1 name2 name3 name4) body) ; Syntax-Pattern
  (let ((name1 (first list-expression))
        (name2 (second list-expression))
        (name3 (third list-expression))
        (name4 (fourth list-expression)))
    body))
               

(module+ test
  (require rackunit)
  (check-equal?
   (destructure (list 1 2 3 4)
                (a b c d) ; neue Namen werden gebunden
                (+ a b c d))
   10))

