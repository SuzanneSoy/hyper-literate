#lang hyper-literate/typed typed/racket/base

@;((curry + 1) 2)

@chunk[<*>
       ;curry ;; should give an error when using typed/racket/base
       ((make-predicate One) 1)
       (ann 'sym Symbol)
       (define (f [x : 'e]) x)
       (ann (f 'e) 'e)]
