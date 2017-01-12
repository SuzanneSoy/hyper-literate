#lang typed/racket

(provide pairof?)

(require typed/racket/unsafe)
(unsafe-require/typed racket/function
                      [[identity unsafe-cast-function] (∀ (A) (→ Any A))])
(define-syntax-rule (unsafe-cast v t)
  ((inst unsafe-cast-function t) v))

(: pairof?* (∀ (A D) (→ Any
                        (→ Any Boolean : A)
                        (→ Any Boolean : D)
                        Boolean)))
(define (pairof?* v a? d?)
  (and (pair? v)
       (a? (car v))
       (d? (cdr v))))

(define pairof?
  ;; Circumvent https://github.com/racket/typed-racket/issues/429
  (unsafe-cast pairof?*
               (∀ (A D) (→ Any
                           (→ Any Boolean : A)
                           (→ Any Boolean : D)
                           Boolean
                           :
                           ;; Circumvent
                           ;; https://github.com/racket/typed-racket/issues/488
                           #:+ (Pairof A D)
                           #:- (! (Pairof A D))))))