#lang typed/racket

(require "annotate-syntax-typed.rkt"
         tr-immutable/typed-syntax
         rackunit)

(require typed/racket/unsafe)
(unsafe-require/typed sexp-diff
                      [sexp-diff (case→
                                  (→ Sexp Sexp Sexp)
                                  (→ Sexp/Non Sexp/Non Sexp/Non)
                                  (→ (Sexpof Any) (Sexpof Any) (Sexpof Any)))])

(provide check-same-syntax)

(: same-syntax! (→ ISyntax/Non ISyntax/Non Boolean))
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