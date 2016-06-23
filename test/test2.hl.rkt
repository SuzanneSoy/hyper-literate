#lang hyper-literate typed/racket/base

@(require (for-label typed/racket/base
                     rackunit))

@title{Title}

Hello world.

@chunk[<*>
       (begin
         ; Wrapped with (begin …) to avoid the implicit require for-label.
         (require typed/rackunit))

       ;; Would give an error as typed/racket/base is used on the #lang line:
       ;curry

       (check-equal? ((make-predicate One) 1) #t)
       
       (define (f [x : 'e123]) x)
       
       (define ee (ann (f 'e123) 'e123))
       (provide ee)]
