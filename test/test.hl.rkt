#lang hyper-literate/typed typed/racket/base

@title{Title}

Hello world.

@(if-preexpanding
  (void)
  (require (submod "..")))

@(unless-preexpanding
  (symbol->string ee))

@chunk[<*>
       (require typed/rackunit)
       (module ms typed/racket/base
         (define x 1)
         (provide x))
       (require 'ms)
       (check-equal? (+ x x) 2)
       ;; Gives an error because typed/racket/base is used on the #lang line:
       ;curry
       (check-equal? ((make-predicate One) 1) #t)
       (check-equal? (ann 'sym Symbol) 'sym)
       (define (f [x : 'e123]) x)
       (define ee (ann (f 'e123) 'e123))
       (provide ee)]
