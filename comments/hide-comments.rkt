#lang racket

(require (rename-in syntax/parse [...+ …+])
         syntax/stx
         racket/match
         racket/set
         racket/list
         racket/function
         racket/vector
         racket/contract
         sexp-diff
         racket/pretty
         rackunit
         (only-in racket/base [... …])
         (for-syntax (rename-in racket/base [... …]))
         "syntax-properties.rkt")

(provide hide-#%comment)

;;    ([#%comment c1] a [#%comment c2] . ([#%comment c3] b [#%comment c4]))
;; => (aᶜ² . (bᶜ⁴)⁻ᶜ³)⁻ᶜ¹
;;    (c1 a c2 . (c3 . (c4 b c5)))
;; => (aᶜ² . (bᶜ⁵)⁻ᶜ³⁻⁻ᶜ⁴)⁻ᶜ¹
;;    (c1 a c2 . (c3 . (c4 c5)))
;; => (aᶜ² . ()⁻ᶜ³⁻⁻ᶜ⁴ᶜ⁵)⁻ᶜ¹
;;    (c1 a (c2) b)
;; => (a ()⁻ᶜ² b)⁻ᶜ¹
;;    (c1 a (c2 . b) c)
;; => (a b⁻ᶜ² c)⁻ᶜ¹
;;    (c1 a (c2 . (c3 c4)) c)
;; => (a ()⁻ᶜ²⁻⁻ᶜ³ᶜ⁴ c)⁻ᶜ¹
(define (hide-#%comment stx)
  (match (syntax-e stx)
    [(not (? pair?))
     ;; TODO: recurse down vectors etc.
     stx]
    [(list* e* ... rest)
     (syntax-parse e*
       #:datum-literals (#%comment)
       [({~and c₀ [#%comment . _]} …
         {~seq {~and eᵢ {~not [#%comment . _]}}
               {~and cᵢⱼ [#%comment . _]} …}
         …+)
        (define new-e* (map with-comments-after
                            (map hide-#%comment
                                 (syntax->list #'(eᵢ …)))
                            (map syntax->list
                                 (syntax->list #'((cᵢⱼ …) …)))))
        (define new-rest (if (null? rest)
                             rest
                             (hide-#%comment rest)))
        (with-first-comments
         (datum->syntax stx (append new-e* new-rest) stx stx)
         (cons #f (syntax->list #'(c₀ …))))]
       [({~and c₀ [#%comment . _]} …)
        (define new-rest (if (null? rest)
                             rest
                             (hide-#%comment rest)))
        (with-first-comments
         (with-comments-after
          (datum->syntax stx new-rest stx stx)
          (if (syntax? new-rest)
              (syntax-property new-rest 'comments-after)
              '()))
         (cons (if (syntax? new-rest)
                   (cons (datum->syntax new-rest
                                        'saved-props+srcloc
                                        new-rest
                                        new-rest)
                         (or (syntax-property new-rest 'first-comments)
                             ;; TODO: I'm dubious about this, better typecheck
                             ;; everything…
                             (cons #f null)))
                   #f)
               (syntax->list #'(c₀ …))))])]))