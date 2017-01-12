#lang typed/racket

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
         tr-immutable/typed-syntax
         "syntax-properties-typed.rkt")

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
(: hide-#%comment (→ ISyntax/Non-Stx ISyntax/Non-Stx))
(define (hide-#%comment stx)
  (cond
    [(pair? (syntax-e stx))
     (hide-in-pair (syntax-e stx))]
    [else
     ;; TODO: recurse down vectors etc.
     stx]))

(define-type ISyntax/Non-List*
  (Rec L (U ISyntax/Non
            Null
            (Pairof ISyntax/Non L))))

(define pair (ann cons (∀ (A B) (→ A B (Pairof A B)))))

(: hide-in-pair (→ ISyntax/Non-List*
                   ISyntax/Non-Stx))
(define (hide-in-pair e*)
  (let loop ([rest : ISyntax/Non-List* e*]
             [groups : (Pairof (Listof Comment)
                               (Listof (Pairof ISyntax/Non (Listof Comment))))
                     '(())])
    (if (pair? rest)
        (if (comment? (car rest))
            (loop (cdr rest)
                  (pair (pair (ann (car rest) Comment) (car groups))
                        (cdr groups)))
            (loop (cdr rest)
                  (pair (ann '() (Listof Comment))
                        (pair (pair (car rest) (reverse (car groups)))
                              (cdr groups)))))
        (values rest groups)))
  (error "TODOrtfdsvc"))

(define-type Comment (Syntaxof (Pairof (Syntaxof '#%comment) Any)))
(define comment? (make-predicate Comment))


#;(if ((make-predicate (Rec R (Pairof (Syntaxof (Pairof (Syntaxof '#%comment) Any))
                                      (U Boolean
                                         Char
                                         Number
                                         Keyword
                                         Null
                                         String
                                         Symbol
                                         BoxTop
                                         VectorTop
                                         R))))
       e*)
      (error "TODOwa" e*)
      (error "TODOwa" e*))

#|
(: listof? (∀ (A) (→ Any (→ Any Boolean : A) Boolean : (Listof A))))
(define (listof? l p?)
  (pair? l
         p?
         (ann (λ (a)
                (list*? a p?))
              (→ Any Boolean : ))
|#

#;(match (syntax-e stx)
    [(not (? pair?))
     ;; TODO: recurse down vectors etc.
     stx]
    [(list* e* ... rest)
     (error "TODO")
     #;(syntax-parse e*
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
                 (syntax->list #'(c₀ …))))])])