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

(struct oder
  (trigger1
   trigger2)
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
     (let ((counter-before (event-counter (find-event snapshot-before event-name)))
           (counter-now (event-counter (find-event snapshot-now event-name))))
       (length
        (filter
         (lambda (counter)
           (and (> counter counter-before)
                (>= counter-now counter)))
         counter-list))))
    ((oder trigger1 trigger2)
     (+ (how-often trigger1 snapshot-before snapshot-now)
        (how-often trigger2 snapshot-before snapshot-now)))))

; Assoziativität
; (oder t1 (oder t2 t3)) == (oder (oder t1 t2) t3)

; Kommutativität
; (oder t1 t2) == (oder t2 t1)

(module+ test
  (require (only-in rackunit check-equal?)) ; after ist in rackunit definiert
  (define snapshot0
    (list (event 'current-order 17) (event 'visual-inspection 3)))
  (define snapshot1
    (list (event 'current-order 500) (event 'visual-inspection 17)))
  (define snapshot2
    (list (event 'current-order 7814) (event 'visual-inspection 87)))
  (check-equal?
   (how-often (every 'current-order 1000) snapshot0 snapshot2)
   7)

  (check-equal?
   (how-often (after 'current-order '(500 1500 5000 10000)) snapshot0 snapshot2)
   3)
  
  ; (+ (how-often trigger snapshot-a snapshot-b)
  ;    (how-often trigger snapshot-b snapshot-c)) ==
  ; (how-often trigger snapshot-a snapshot-c)

  (check-equal?
   (how-often (after 'current-order '(500 1500 5000 10000)) snapshot0 snapshot1)
   1)
  (check-equal?
   (how-often (after 'current-order '(500 1500 5000 10000)) snapshot1 snapshot2)
   2)

  (check-equal?
   (how-often (oder (every 'current-order 1000) (every 'visual-inspection 10))
              snapshot0 snapshot2)
   15))
              
                    
              
              