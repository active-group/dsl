#lang racket/base
(require racket/contract
         (only-in racket/function identity))

(struct direction (right down) #:transparent)
(define right (direction 1 0))
(define down (direction 0 1))
(define here (direction 0 0))

(struct position (x y) #:transparent)

(define (move-position pos direction)
  (position (+ (position-x pos) (direction-right direction))
            (+ (position-y pos) (direction-down direction))))

(struct cell-format (parse) #:transparent)

(struct constant-format (text) #:transparent)

(struct record-format (constructor direction field-formats) #:transparent)

(struct list-format (direction element-format) #:transparent)

(define format/c (or/c cell-format? constant-format? record-format? list-format?))

(define (ignore direction fields)
  (record-format (lambda values
                   #f)
                 direction
                 fields))

(define (choose index direction fields)
  (record-format (lambda (values)
                   (list-ref values index))
                 direction
                 fields))

(define text
  (cell-format
   (lambda (content)
     (cond
       ((string? content) content)
       ((number? content) (number->string content))))))

(define number
  (cell-format
   (lambda (content)
     (cond
       ((string? content) (string->number content)
       ((number? content) content))))))
  

; TODOs:
; - type system for formats, avoiding overlap
; - macros for better DSL
; - surface syntax

(define table/c (integer? integer? . -> . any))

(define (table-ref table position)
  (table (position-x position)
         (position-y position)))

(define result/c any/c) ; FIXME

(define (parse-table format table position)
  (cond
    ((cell-format? format)
     ((cell-format-parse format) (table-ref table position)))
    ((constant-format? format)
     (let ((contents (table-ref table position))
           (expected (constant-format-text format)))
       (if (string=? contents expected)
           contents
           (error 'parse-table "found contents ~a, expected ~a" contents expected))))
    ((record-format? format)
     (let ((direction (record-format-direction format)))
       (let loop ((field-formats (record-format-field-formats format))
                  (position position)
                  (reverse-parsed '()))
         (if (null? field-formats)
             (apply (record-format-constructor format) (reverse reverse-parsed))
             (let ((parsed (parse-table (car field-formats) table position)))
               (loop (cdr field-formats)
                     (move-position position direction) 
                     (cons parsed reverse-parsed)))))))
    ((list-format? format)
     (let ((direction (list-format-direction format))
           (format (list-format-element-format format)))
       (let loop ((position position)
                  (reverse-elements '()))
         (with-handlers
             ((exn:fail?
               (lambda (exn)
                 (reverse reverse-elements))))
           (loop (move-position position direction)
                 (cons (parse-table format table position)
                       reverse-elements))))))))

(provide
 (contract-out
  (struct direction ((right integer?) (down integer?)))
  (struct position ((x integer?) (y integer?)))
  (struct cell-format ((parse (string? . -> . any))))
  (struct constant-format ((text string?)))
  (struct record-format
    ((constructor procedure?) ; sloppy
     (direction direction?)
     (field-formats (listof format/c))))
  (struct list-format
    ((direction direction?)
     (element-format format/c)))
  
  (text format/c)
  (number format/c)


  (choose (integer? direction? (listof format/c) . -> . format/c))
  (ignore (direction? (listof format/c) . -> . format/c))

  (table/c contract?)

  (table-ref (table/c position? . -> . any))

  (parse-table (format/c table/c position? . -> . result/c))))
  
  

(module+ test
  (require rackunit)

  (check-equal?
   (parse-table (cell-format string->number)
                (lambda (x y) "123")
                (position 0 0))
   123)
  (check-equal?
   (parse-table (constant-format "Mike")
                (lambda (x y) "Mike")
                (position 0 0))
   "Mike")
  
  (check-exn
   (lambda (exn)
     (string=? (exn-message exn)
               "parse-table: found contents Kaan, expected Mike"))
   (lambda ()
     (parse-table (constant-format "Mike")
                  (lambda (x y) "Kaan")
                  (position 0 0))
     "Mike"))

  
  (struct entry (name address phone)
    #:transparent)

  (define (lists->table llist)
    (lambda (x y)
      (list-ref (list-ref llist y) x)))

  (check-equal?
   (parse-table (record-format entry
                               right
                               (list (cell-format identity)
                                     (cell-format identity)
                                     (cell-format string->number)))
                (lists->table '(("Mike" "Pappelweg" "123")))
                (position 0 0))
   (entry "Mike" "Pappelweg" 123))

  (check-equal?
   (parse-table (list-format right (cell-format string->number))
                (lists->table '(("1" "2" "3")))
                (position 0 0))
   '(1 2 3))
  
  (define format1
    (record-format
     (lambda (heading list) list)
     down
     (list
      (ignore
       right
       (list (constant-format "Name")
             (constant-format "Address")
             (constant-format "Phone")))
      (list-format down
                   (record-format entry
                                  right
                                  (list (cell-format identity)
                                        (cell-format identity)
                                        (cell-format string->number)))))))



  (define table1
    (lists->table
     '(("Name" "Address" "Phone")
       ("Mike" "Pappelweg" "162")
       ("Kaan" "Hannover" "511"))))

  (check-equal?
   (parse-table format1 table1 (position 0 0))
   (list (entry "Mike" "Pappelweg" 162)
         (entry "Kaan" "Hannover" 511)))


  ;;; Read and parse XLSX file
  ;; - Install simple-xlsx via `raco pkg install simple-xlsx`

  ;; simple-xlsx casts integers already to ints. we have to undo that
  ;; to make it work with `format1`
  (define (llist->llist-of-strings llis)
    (map (lambda (lis-of-thingys)
           (map (lambda (thing)
                  (cond
                    ((number? thing) (number->string thing))
                    ((string? thing) thing)
                    (#t "warning:  not a number, not a string")))
                lis-of-thingys))
         llis))

  (require simple-xlsx)
  (read-xlsx "read.xlsx"
             (lambda ()
               (check-equal? (get-sheet-name-list) '("Addresses"))
               (with-sheet-name
                 "Addresses"
                 (lambda ()
                   (let ((content (llist->llist-of-strings (get-rows))))
                     (check-equal?
                      (parse-table format1 (lists->table content) (position 0 0))
                      (list (entry "Mike" "Tübingen" 123)
                            (entry "Kaan" "Hannover" 987))))))))

  )
