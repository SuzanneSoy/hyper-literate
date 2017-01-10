#lang racket

(require "annotate-syntax.rkt"
         sexp-diff
         rackunit)

(provide check-same-syntax)

(define (same-syntax! a b)
  (define answer (equal? (annotate-syntax a #:srcloc+scopes? #f)
                         (annotate-syntax b #:srcloc+scopes? #f)))
  (unless answer
    (pretty-write
     (sexp-diff (annotate-syntax a)
                (annotate-syntax b)))
    (displayln a)
    (displayln b))
  answer)

(define-syntax (check-same-syntax stx)
  (syntax-case stx ()
    [(_ a b)
     (datum->syntax #'here
                    `(check-true (same-syntax! ,#'a ,#'b))
                    stx)]))