#lang racket
; Torbens Trigger

; Trigger ist eins der folgenden:
; - After -ODER-
; - Every -ODER-
; - Or -ODER-
; - And -ODER-
; - Reset-Gruppe

(struct every
  (event-name
   interval)
  #:transparent)

(struct after
  (event-name
   counter-list)
  #:transparent)

; Event besteht aus:
; - Name
; - Zähler
(struct event
  (name ; Symbol 'produced-quantity, 'visual-inspection
   counter)
  #:transparent)

; Schnappschuß: Liste von Events
(define (find-event snapshot name)
  (findf (lambda (event)
           (equal? (event-name event)
                   name))
         snapshot))

; Wie oft feuert der Trigger?
(define (how-often trigger snapshot-before snapshot-now)
  (match trigger
    ((every event-name interval)
     (let ((counter-before (event-counter (find-event snapshot-before event-name)))
           (counter-now (event-counter (find-event snapshot-now event-name))))
       (quotient (- counter-now counter-before) ; ganzzahlige Division
                 interval)))
    ((after event-name counter-list)
     (let ((counter-now (event-counter (find-event snapshot-now event-name))))
       (length
        (filter
         (lambda (counter)
           (>= counter-now counter))
         counter-list))))))

(module+ test
  (require (only-in rackunit check-equal?)) ; after ist in rackunit definiert
  (define snapshot0
    (list (event 'current-order 17)))
  (define snapshot1
    (list (event 'current-order 7814)))
  (check-equal?
   (how-often (every 'current-order 1000) snapshot0 snapshot1)
   7)

  (check-equal?
   (how-often (after 'current-order '(500 1500 5000 10000)) snapshot0 snapshot1)
   3))
              
              