#lang racket

; Der Einflußbereich eines Schiffs ("shape", "area") ist eins der folgenden:
; - Kreis
; - Quadrat
; - eine Überlagerung zweier Einflußbereiche

; 1. Datenmodell dafür
; 2. Funktion, die feststellt, ob ein Punkt innerhalb oder außerhalb eines Einflußbereichs ist

(struct position
  (x
   y)
  #:transparent)

(struct area
  ())

(struct circle area
  (center
   radius)
  #:transparent)

(struct quadrat area
  (center
   length)
  #:transparent)

(struct combinedArea area
  (area1
   area2)
  #:transparent)

; Wurzel((position.x - circle.center.position.x)^2 + (position.y - circle.center.position.y)^2)
(define (isInCircle? pos circ)
  (<=
   (sqrt (
          + (expt
             (- (position-x pos)
                (position-x (circle-center circ)))
             2)
            (expt
             (- (position-y pos)
                (position-y (circle-center circ)))
             2)))
   (circle-radius circ)))


(module+ test
  (require rackunit)

  (define circlePosition (position 0 0))
  (define testCirclePosition (position 2 2))

  (define circle1 (circle testCirclePosition 10))
  
  (check-equal? (isInCircle? testCirclePosition circle1)
                #t)

  (define testCirclePosition2 (position 20 20))

  (check-equal? (isInCircle? testCirclePosition2 circle1)
                #f))

(define (isInQuadrat? pos quad)
  (<=
   (+ (abs
       (- (position-x pos)
          (position-x (quadrat-center quad))))
      (abs
       (- (position-y pos)
          (position-y (quadrat-center quad)))))
   (/ (quadrat-length quad) 2)))

(module+ test
  (require rackunit)

  (define quadratPosition (position 0 0))
  (define testQuadratPosition (position 2 2))

  (define quadrat1 (quadrat testQuadratPosition 10))
  
  (check-equal? (isInQuadrat? testQuadratPosition quadrat1)
                #t)

  (define testQuadratPosition2 (position 30 30))

  (check-equal? (isInQuadrat? testQuadratPosition2 quadrat1)
                #f))

(define (isIn? pos a)
    (match a
      ((circle center radius) (isInCircle? pos a))
      ((quadrat center length) (isInQuadrat? pos a))
      ((combinedArea area1 area2) (or (isIn? pos area1) (isIn? pos area2)))))

(module+ test
  (require rackunit)

  (check-equal? (isIn? testCirclePosition circle1)
                #t)

  (check-equal? (isIn? testCirclePosition2 circle1)
                #f)
  
  (check-equal? (isIn? testQuadratPosition quadrat1)
                #t)

  (check-equal? (isIn? testQuadratPosition2 quadrat1)
                #f)

  (define combinedArea1 (combinedArea circle1 quadrat1))
  (define combinedArea2 (combinedArea quadrat1 circle1))

  (check-equal? (isIn? testQuadratPosition2 combinedArea1)
                #f)
  (check-equal? (isIn? testQuadratPosition2 combinedArea2)
                #f))