#lang typed/racket

(require typed-map
         "../../comments/typed-syntax.rkt")

(provide annotate-syntax)

(: annotate-syntax (->* (Syntax)
                        (#:srcloc+scopes? Boolean)
                        Sexp))
(define (annotate-syntax e #:srcloc+scopes? [srcloc+scopes? #f])
  (annotate-syntax1 e srcloc+scopes?))

(: annotate-syntax1 (→ (U Syntax Syntax-E)
                       Boolean
                       Sexp))
(define (annotate-syntax1 e srcloc+scopes?)
  (cond
    [(syntax? e)
     (append
      (list 'syntax
            (append-map (λ ([kᵢ : Symbol])
                          (if (and (or (eq? kᵢ 'first-comments)
                                       (eq? kᵢ 'comments-after))
                                   (not (syntax-property e kᵢ)))
                              (list)
                              (list kᵢ (any->isexp (syntax-property e kᵢ)))))
                        (syntax-property-symbol-keys e)))
      (if srcloc+scopes?
          (list (any->isexp (syntax-source e))
                (any->isexp (syntax-line e))
                (any->isexp (syntax-column e))
                (any->isexp (syntax-position e))
                (any->isexp (syntax-span e))
                (any->isexp (syntax-source-module e))
                (any->isexp (hash-ref (syntax-debug-info e)
                                      'context)))
          (list))
      (list (annotate-syntax1 (syntax-e e) srcloc+scopes?)))]
    [(null? e)
     'null]
    [(list? e)
     (list 'list
           (map (λ (eᵢ) (annotate-syntax1 eᵢ srcloc+scopes?))
                e))]
    [(pair? e)
     (list 'cons
           (annotate-syntax1 (car e) srcloc+scopes?)
           (annotate-syntax1 (cdr e) srcloc+scopes?))]
    [(vector? e)
     (list 'vector
           (immutable? e)
           (map (λ (eᵢ) (annotate-syntax1 eᵢ srcloc+scopes?))
                (vector->list e)))]
    [(symbol? e)
     e]
    [(string? e)
     e]
    [else
     (raise-argument-error
      'annotate-syntax
      (string-append "a syntax object containing recursively on of the"
                     " following: pair, null, vector, symbol, string")
      0
      e)]))