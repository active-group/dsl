#lang racket/base
(require racket/contract
         racket/match)

(struct node (srcloc) #:transparent)

(struct node-list node (items) #:transparent)

(struct item node (item) #:transparent)



(struct table node (name headers types fields)
  #:transparent)

(provide (struct-out node)
         (struct-out table)
         (struct-out item))


#;(struct headers (headers-list)
  #:transparent)

