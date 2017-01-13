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
               "scribble-enhanced"
               "sexp-diff"
               "tr-immutable"
               "typed-map-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"
                     "rackunit-doc"
                     "scribble-doc"))
(define scribblings '(("scribblings/hyper-literate.scrbl" ())
                      ("test/test.hl.rkt" () (omit-start))
                      ("test/test2.hl.rkt" () (omit-start))))
(define pkg-desc
  (string-append "Hyper-literate programming is to literate programming exactly"
                 " what hypertext documents are to regular books and texts."
                 " For now, this is based on scribble/lp2, and only contains"
                 " some ε-improvements over it"))
(define version "0.2")
(define pkg-authors '(|Georges Dupéron|))
