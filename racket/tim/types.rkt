#lang racket
(require tim/tim)

(struct direction-type () #:transparent)
(struct position-type () #:transparent)

#|
The idea is that formats have an *extent* which is essentially the dimensions
of its bounding box.  The extent can also be infite however because of lists.
|#

(struct format-type (right-extent down-extent) #:transparent)

; note this probably can't be zero
(define extent/c (or/c integer? +inf.0 -inf.0))

(define one-extent (format-type 1 1))

; right and down are the "components" or a direction

(define (type-of-format format)
  (cond
    ((cell-format? format) one-extent)
    ((constant-format? format) one-extent)
    ((record-format? format)
     (let ((direction (record-format-direction format)))
       (let ((right (direction-right direction))
             (down (direction-down direction)))
         (let loop ((field-formats (record-format-field-formats format))
                    (right-extent 0)
                    (down-extent 0))
           (if (null? field-formats)
               (format-type right-extent down-extent)
               (let ((field-format-type (type-of-format (car field-formats))))
                 (let ((field-right-extent (format-type-right-extent field-format-type))
                       (field-down-extent (format-type-down-extent field-format-type)))
                   (loop (cdr field-formats)
                         (extent-extend right-extent
                                        right
                                        field-right-extent)
                         (extent-extend down-extent
                                        down
                                        field-down-extent)))))))))
                  
    ((list-format? format)
     (let ((direction (list-format-direction format))
           (element-format-type (type-of-format (list-format-element-format format))))
       (format-type (list-format-extent (direction-right direction)
                                        (format-type-right-extent element-format-type))
                    (list-format-extent (direction-down direction)
                                        (format-type-down-extent element-format-type)))))))

; for records

#|
direction: 0
if it's an integer, the signs need to agree and we do the minimum/maximum

direction: some number d
extent-so-far can't be infinite then, just need to make sure d moves past
|#

(define (extent-extend extent-so-far ; extent of fields to the left
                       component ; direction component
                       field-extent)  ; extent of the field we're looking at
  (cond
    ((zero? component)
     (cond
       ((>= extent-so-far 0)
        (unless (>= field-extent 0)
          (error 'extent-extend "record extends in two directions: ~a and ~a" extent-so-far field-extent))
        (max extent-so-far field-extent))
       (else
        (unless (< field-extent 0)
          (error 'extent-extend "record extends in two directions: ~a and ~a" extent-so-far field-extent))
        (min extent-so-far field-extent))))
    ((not (integer? field-extent))
     (error 'extent-extend "field extent can't be infinite in the record's direction"))
    ((positive? component)
     (unless (>= field-extent 0)
       (error 'extent-extend "record moves in a different direction ~a from its field ~a" direction field-extent))
     (+ extent-so-far component))
    ((negative? component)
     (unless (<= field-extent 0)
       (error 'extent-extend "record moves in a different direction ~a from its field ~a" direction field-extent))
     (+ extent-so-far component))))

(define (list-format-extent component element-format-extent)
  (cond
    ((zero? component)
     element-format-extent)
    ((not (integer? element-format-extent))
     (error 'list-format-extent "can't have a list element format ~a be infinite in the direction the list is going ~a"
            component element-format-extent))
    ((positive? component)
     (unless (>= element-format-extent 0)
       (error 'list-format-extent "the list element extends in direction ~a, the other way around from the list ~a"
              component element-format-extent))
     +inf.0)
    ((negative? component)
     (unless (<= element-format-extent 0)
       (error 'list-format-extent "the list element extends in direction ~a, the other way around from the list ~a"
              component element-format-extent))
     -inf.0)))



(provide
 (contract-out
  (extent/c contract?)
  (struct direction-type ())
  (struct position-type ())
  (struct format-type ((right-extent extent/c)
                       (down-extent extent/c)))))

(module+ test
  (require rackunit)

  (check-equal? (type-of-format (cell-format string->number))
                (format-type 1 1))
  (check-equal? (type-of-format (constant-format "Mike"))
                (format-type 1 1))


  (struct entry (name address phone)
    #:transparent)

  (define right (direction 1 0))
  (define down (direction 0 1))
  (define here (direction 0 0))

  (check-equal? (type-of-format (record-format entry
                                               right
                                               (list (cell-format identity)
                                                     (cell-format identity)
                                                     (cell-format string->number))))
                (format-type 3 1))
  
  (check-equal? (type-of-format (list-format down
                                             (record-format entry
                                                            right
                                                            (list (cell-format identity)
                                                                  (cell-format identity)
                                                                  (cell-format string->number)))))
                (format-type 3 +inf.0))

  )

