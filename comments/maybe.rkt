#lang typed/racket
(provide (struct-out Some)
         Maybe)

(struct (A) Some ([v : A]) #:prefab)
(define-type (Maybe A)
  (U (Some A) #f))