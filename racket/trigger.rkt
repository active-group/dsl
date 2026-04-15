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

(struct after-except ; wie after, nur dann auslassen, wenn manual ausgelöst
  (event-name
   counter-list
   max-delta) ; maximale Abweichung im Zeitpunkt
  #:transparent)

(struct oder
  (trigger1
   trigger2)
  #:transparent)

(struct und
  (trigger1
   trigger2)
  #:transparent)

#;(struct nicht
    (trigger)
    #:transparent)

#;(struct reset-group
    (conditional-trigger
     trigger)
    #:transparent)


; (reset-group manual1 (after 't '(100 500 1000 5000))
; (reset-group manual1 (oder (after 't '(100 500 1000 5000))
;                            (every 'u 1000)))
; manual1 löst aus bei 999

#;(struct implication
    (conditional-trigger
     trigger)
    #:transparent)

; (implication (nicht manual1) (after 't '(100 500 1000 5000)))
; 
                      

; Event besteht aus:
; - Name
; - Zähler
(struct event
  (name ; Symbol 'produced-quantity, 'visual-inspection
   counter)
  #:transparent)

(struct manual
  (event-name
   counter) ; Zähler zum Zeitpunkt der Auslösung
  #:transparent)

; Schnappschuß: Liste von Events
(define (find-event snapshot name)
  (findf (lambda (event)
           (equal? (event-name event)
                   name))
         snapshot))

; Semantik
; Wie oft feuert der Trigger?
; manuals: Liste von manual
; Interpreter
#;(define (how-often trigger snapshot-before snapshot-now manuals)
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
      ((after-except event-name counter-list max-delta)
       (let ((counter-before (event-counter (find-event snapshot-before event-name)))
             (counter-now (event-counter (find-event snapshot-now event-name))))
         (length
          (filter
           (lambda (counter)
             (and (not (findf (lambda (manual)
                                (and (equal? (manual-event-name manual) event-name)
                                     (<= (abs (- (manual-counter manual) counter))
                                         max-delta)))
                              manuals))
                  (> counter counter-before)
                  (>= counter-now counter)))
           counter-list))))
      ((oder trigger1 trigger2)
       (+ (how-often trigger1 snapshot-before snapshot-now manuals)
          (how-often trigger2 snapshot-before snapshot-now manuals)))
      ((und trigger1 trigger2)
       (* (how-often trigger1 snapshot-before snapshot-now manuals)
          (how-often trigger2 snapshot-before snapshot-now manuals)))
      #;((nicht trigger)
         (write (list 'nicht trigger snapshot-before snapshot-now (how-often trigger snapshot-before snapshot-now))) (newline)
         (if (zero? (how-often trigger snapshot-before snapshot-now))
             1 ; ???
             0))
      #;((implication condition-trigger trigger)
         (if (not (zero? (how-often condition-trigger snapshot-before snapshot-now)))
             (how-often trigger snapshot-before snapshot-now)
             0))))

(define (how-often trigger snapshot-before snapshot-now manuals)
  ((how-often* trigger) snapshot-before snapshot-now manuals))

; Compiler
(define (how-often* trigger)
  (match trigger
    ((every event-name interval)
     (lambda (snapshot-before snapshot-now manuals)
       (let ((counter-before (event-counter (find-event snapshot-before event-name)))
             (counter-now (event-counter (find-event snapshot-now event-name))))
         (quotient (- counter-now counter-before) ; ganzzahlige Division
                   interval))))
    ((after event-name counter-list)
     (lambda (snapshot-before snapshot-now manuals)
       (let ((counter-before (event-counter (find-event snapshot-before event-name)))
             (counter-now (event-counter (find-event snapshot-now event-name))))
         (length
          (filter
           (lambda (counter)
             (and (> counter counter-before)
                  (>= counter-now counter)))
           counter-list)))))
    ((after-except event-name counter-list max-delta)
     (lambda (snapshot-before snapshot-now manuals)
       (let ((counter-before (event-counter (find-event snapshot-before event-name)))
             (counter-now (event-counter (find-event snapshot-now event-name))))
         (length
          (filter
           (lambda (counter)
             (and (not (findf (lambda (manual)
                                (and (equal? (manual-event-name manual) event-name)
                                     (<= (abs (- (manual-counter manual) counter))
                                         max-delta)))
                              manuals))
                  (> counter counter-before)
                  (>= counter-now counter)))
           counter-list)))))
    ((oder trigger1 trigger2)
     (let ((how-often-trigger1 (how-often* trigger1))
           (how-often-trigger2 (how-often* trigger2)))
       (lambda (snapshot-before snapshot-now manuals)
         (+ (how-often-trigger1 snapshot-before snapshot-now manuals)
            (how-often-trigger2 snapshot-before snapshot-now manuals)))))
    ((und trigger1 trigger2)
     (let ((how-often-trigger1 (how-often* trigger1))
           (how-often-trigger2 (how-often* trigger2)))
       (lambda (snapshot-before snapshot-now manuals)
         (* (how-often-trigger1 snapshot-before snapshot-now manuals)
            (how-often-trigger2 snapshot-before snapshot-now manuals)))))))

; Assoziativität
; (oder t1 (oder t2 t3)) == (oder (oder t1 t2) t3)

; Kommutativität
; (oder t1 t2) == (oder t2 t1)

; Distributivität
; (and a (or b c)) = (or (and a b) (and a c))

(module+ test
  (require (only-in rackunit check-equal?)) ; after ist in rackunit definiert
  (define snapshot0
    (list (event 'current-order 17) (event 'visual-inspection 3)))
  (define snapshot1
    (list (event 'current-order 500) (event 'visual-inspection 17)))
  (define snapshot2
    (list (event 'current-order 7814) (event 'visual-inspection 87)))
  (check-equal?
   (how-often (every 'current-order 1000) snapshot0 snapshot2 '())
   7)

  (check-equal?
   (how-often (after 'current-order '(500 1500 5000 10000)) snapshot0 snapshot2 '())
   3)
  
  ; (+ (how-often trigger snapshot-a snapshot-b)
  ;    (how-often trigger snapshot-b snapshot-c)) ==
  ; (how-often trigger snapshot-a snapshot-c)

  (check-equal?
   (how-often (after 'current-order '(500 1500 5000 10000)) snapshot0 snapshot1 '())
   1)
  (check-equal?
   (how-often (after 'current-order '(500 1500 5000 10000)) snapshot1 snapshot2 '())
   2)

  (check-equal?
   (how-often (after-except 'current-order '(500 1500 5000 10000) 100) snapshot0 snapshot2
              (list (manual 'current-order 1490)))
   2)
   
  (check-equal?
   (how-often (oder (every 'current-order 1000) (every 'visual-inspection 10))
              snapshot0 snapshot2 '())
   15)

  #;(define manual1 (after 'manual '(1)))
  #;(check-equal?
     (how-often (implication (nicht manual1)
                             (every 'current-order 1000))
                snapshot0
                snapshot2)
     0))
              
                    
              
