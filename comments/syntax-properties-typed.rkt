#lang typed/racket

(provide First-Comments
         Comments-After
         with-first-comments
         with-comments-after
         extract-first-comments
         extract-comments-after)

(require "typed-syntax.rkt")

(define-type First-Comments
  (Rec R (Pairof (U #f (Pairof (Syntaxof 'saved-props+srcloc)
                               R))
                 (Listof ISyntax))))

(define-type Comments-After
  (Listof ISyntax))

(: with-first-comments (∀ (A) (→ ISyntax
                                 (U #f First-Comments)
                                 ISyntax)))
(define (with-first-comments e c)
    
  (if (or (not c) (and (= (length c) 1) (not (first c))))
      e
      (syntax-property e 'first-comments c)))

(: with-comments-after (∀ (A) (→ (Syntaxof A)
                                 (U #f Comments-After)
                                 (Syntaxof A))))
(define (with-comments-after e c)
  (if (or (not c) (null? c))
      e
      (syntax-property e 'comments-after c)))

(: extract-first-comments (-> (Syntaxof Any) (U #f First-Comments)))
(define (extract-first-comments stx)
  (define c (syntax-property stx 'first-comments))
  (if ((make-predicate First-Comments) c)
      c
      #f))

(: extract-comments-after (-> (Syntaxof Any) (U #f Comments-After)))
(define (extract-comments-after stx)
  (define c (syntax-property stx 'comments-after))
  (if ((make-predicate Comments-After) c)
      c
      #f))