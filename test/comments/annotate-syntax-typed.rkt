#lang typed/racket

(require typed-map
         tr-immutable/typed-syntax)

(provide annotate-syntax)

(: annotate-syntax (->* (ISyntax/Non)
                        (#:srcloc+scopes? Boolean)
                        Sexp/Non))
(define (annotate-syntax e #:srcloc+scopes? [srcloc+scopes? #f])
  (annotate-syntax1 e srcloc+scopes?))

(: annotate-syntax1 (→ (U ISyntax/Non ISyntax/Non-E)
                       Boolean
                       Sexp/Non))
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
                              (list kᵢ (any->isexp/non (syntax-property e kᵢ)))))
                        (syntax-property-symbol-keys e)))
      (if srcloc+scopes?
          (list (any->isexp/non (syntax-source e))
                (any->isexp/non (syntax-line e))
                (any->isexp/non (syntax-column e))
                (any->isexp/non (syntax-position e))
                (any->isexp/non (syntax-span e))
                (any->isexp/non (syntax-source-module e))
                (any->isexp/non (hash-ref (syntax-debug-info e)
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
    [(box? e)
     (list 'box
           (immutable? e)
           (annotate-syntax1 (unbox e) srcloc+scopes?))]
    [(or (symbol? e)
         (string? e)
         (boolean? e)
         (char? e)
         (number? e)
         (keyword? e))
     e]
    [(NonSyntax? e)
     (list 'NonSyntax (NonSexp (NonSyntax-v e)))]
    [(NonSexp? e)
     (list 'NonSexp e)]))