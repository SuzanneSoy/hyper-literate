#lang info
(define collection "hyper-literate")
(define deps '("base"
               "rackunit-lib"
               "at-exp-lib"
               "scheme-lib"
               "scribble-lib"
               "typed-racket-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/hyper-literate.scrbl" ())
                      ("test/test.hl.rkt" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(|Georges Dupéron|))
