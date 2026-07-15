#lang racket
(require "../tim.rkt")

(struct entry (name address phone)
  #:transparent)

(define right (direction 1 0))
(define down (direction 0 1))

(define entry-row
  (record-format entry right (list text text number)))
(define entries
  (list-format down entry-row))
(define heading
  (ignore right
          (list (constant-format "Name")
                (constant-format "Address")
                (constant-format "Phone"))))
(define addressbook
  (choose 2 down (list heading entries)))


