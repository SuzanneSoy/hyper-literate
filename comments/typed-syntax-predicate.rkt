#lang typed/racket

(provide isexp?
         CoreSexp)

(module unsafe racket
  (provide isexp?)

  (define isexp/c
    (flat-rec-contract isexp
                       (or/c boolean?
                             char?
                             number?
                             keyword?
                             null?
                             (and/c string? immutable?)
                             symbol?
                             (box/c isexp #:immutable #t)
                             (cons/c isexp isexp)
                             (vectorof isexp #:immutable #t))))
  
  (define sexp/c
    (recursive-contract
     (or/c boolean?
           char?
           number?
           keyword?
           null?
           string?
           symbol?
           (box/c sexp/c)
           (cons/c sexp/c sexp/c)
           (vectorof sexp/c))))

  (define isexp?
    (flat-contract-predicate isexp/c)))

(define-type CoreSexp (Rec core-sexp
                           (U Boolean
                              Char
                              Number
                              Keyword
                              Null
                              String
                              Symbol
                              #|(Boxof sexp)|#
                              (Pairof core-sexp core-sexp)
                              #|(Vectorof sexp)|#)))

(require typed/racket/unsafe)
(unsafe-require/typed 'unsafe
                      [isexp? (→ Any Boolean : #:+ Sexp #:- (! CoreSexp))])