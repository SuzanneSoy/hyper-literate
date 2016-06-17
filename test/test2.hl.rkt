#lang hyper-literate typed/racket/base

@(require (for-label typed/racket/base
                     typed/rackunit))

@title{Title}

Hello world.

@chunk[<*>
       (require typed/rackunit)

       ;; Would give an error as typed/racket/base is used on the #lang line:
       ;curry

       (check-equal? ((make-predicate One) 1) #t)
       
       (define (f [x : 'e123]) x)
       
       (define ee (ann (f 'e123) 'e123))
       (provide ee)]
