#lang racket

(provide annotate-syntax)

(define (annotate-syntax e #:srcloc+scopes? [srcloc+scopes? #f])
  (cond
    [(syntax? e)
     (append
      (list 'syntax
            (append-map (λ (kᵢ)
                          (if (and (or (eq? kᵢ 'first-comments)
                                       (eq? kᵢ 'comments-after))
                                   (not (syntax-property e kᵢ)))
                              (list)
                              (list kᵢ (syntax-property e kᵢ))))
                        (syntax-property-symbol-keys e)))
      (if srcloc+scopes?
          (list (syntax-source e)
                (syntax-line e)
                (syntax-column e)
                (syntax-position e)
                (syntax-span e)
                (syntax-source-module e)
                (hash-ref (syntax-debug-info e) 'context))
          (list))
      (list (annotate-syntax (syntax-e e) #:srcloc+scopes? srcloc+scopes?)))]
    [(null? e)
     'null]
    [(list? e)
     (list 'list
           (map (λ (eᵢ) (annotate-syntax eᵢ #:srcloc+scopes? srcloc+scopes?))
                e))]
    [(pair? e)
     (list 'cons
           (annotate-syntax (car e) #:srcloc+scopes? srcloc+scopes?)
           (annotate-syntax (cdr e) #:srcloc+scopes? srcloc+scopes?))]
    [(vector? e)
     (list 'vector
           (immutable? e)
           (map (λ (eᵢ) (annotate-syntax eᵢ #:srcloc+scopes? srcloc+scopes?))
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