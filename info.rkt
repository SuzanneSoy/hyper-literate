#lang info
(define collection "hyper-literate")
(define deps '("base"
               "rackunit-lib"
               "at-exp-lib"
               "scheme-lib"
               "scribble-lib"
               "typed-racket-lib"
               "typed-racket-more"
               "typed-racket-doc"
               "scribble-enhanced"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-doc"
                     "scribble-doc"))
(define scribblings '(("scribblings/hyper-literate.scrbl" ())
                      ("test/test.hl.rkt" () (omit-start))
                      ("test/test2.hl.rkt" () (omit-start))))
(define pkg-desc "Description Here")
(define version "0.1")
(define pkg-authors '(|Georges Dupéron|))
