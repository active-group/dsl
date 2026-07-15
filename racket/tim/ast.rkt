#lang racket/base
(require racket/contract)
        
(define srcloc/c
  (or/c #f
        srcloc?))

(struct node (srcloc) #:transparent)

; top-level forms

(struct defrecord node (name fields) #:transparent)
  
(struct equation node (name rhs) #:transparent)

(define declaration/c
  (or/c defrecord? equation?))

; expressions
(struct record node (name direction field-formats) #:transparent)

(struct list node (direction element-format) #:transparent)

(struct ignore node (direction field-formats) #:transparent)

(struct choose node (index direction formats) #:transparent)

(struct direction node (right down) #:transparent)

(struct coordinates node (x y) #:transparent)

(struct reference node (name) #:transparent)

(struct constant node (text) #:transparent)

(define expression/c
  (or/c record? list? ignore? choose? direction? coordinates? reference? constant?))

; program
(struct program node (declarations format-name coordinates) #:transparent)

(provide
 (contract-out
  (srcloc/c contract?)
  (declaration/c contract?)
  (expression/c contract?)
  (struct node ((srcloc srcloc/c)))
  (struct defrecord ((srcloc srcloc/c)
                     (name string?)
                     (fields (listof string?))))
  (struct equation ((srcloc srcloc/c)
                    (name string?)
                    (rhs expression/c)))

  (struct record ((srcloc srcloc/c)
                  (name string?)
                  (direction expression/c)
                  (field-formats (listof expression/c))))
  (struct list ((srcloc srcloc/c)
                (direction expression/c)
                (element-format expression/c)))
  (struct ignore ((srcloc srcloc/c)
                  (direction expression/c)
                  (field-formats (listof expression/c))))
  (struct choose ((srcloc srcloc/c)
                  (index integer?)
                  (direction expression/c)
                  (formats (listof expression/c))))
  (struct direction ((srcloc srcloc/c)
                     (right integer?)
                     (down integer?)))
  (struct coordinates ((srcloc srcloc/c)
                       (x integer?)
                       (y integer?)))
  (struct reference ((srcloc srcloc/c)
                     (name string?)))
  (struct constant ((srcloc srcloc/c)
                    (text string?)))
  (struct program ((srcloc srcloc/c)
                   (declarations (listof declaration/c))
                   (format-name string?)
                   (coordinates coordinates?)))))

  
