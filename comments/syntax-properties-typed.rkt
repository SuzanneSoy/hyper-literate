#lang typed/racket

(provide First-Comments
         Comments-After
         with-first-comments
         with-comments-after
         extract-first-comments
         extract-comments-after)

(require "typed-syntax.rkt"
         "typed-pairof-predicate.rkt"
         typed-map)

(define-type First-Comments
  (Rec R (Pairof (U #f (Pairof (Syntaxof 'saved-props+srcloc)
                               R))
                 (Listof ISyntax))))

(define-type Comments-After
  (Listof ISyntax))

(: first-comments? (→ Any Boolean : (Pairof (U #f (Pairof (Syntaxof 'saved-props+srcloc)
                                                          First-Comments))
                                            (Listof ISyntax))))
(define (first-comments? v)
  (define p? (inst pairof?
                   (U #f (Pairof (Syntaxof 'saved-props+srcloc)
                                 First-Comments))
                   (Listof ISyntax)))
  (p? v first-comments1? first-comments2?))

(: first-comments1? (→ Any Boolean : (U #f (Pairof (Syntaxof 'saved-props+srcloc)
                                                   First-Comments))))
(define (first-comments1? v)
  (or (false? v)
      (first-comments11? v)))

(: first-comments11? (→ Any Boolean : (Pairof (Syntaxof 'saved-props+srcloc)
                                              First-Comments)))
(define (first-comments11? v)
  (define p? (inst pairof?
                   (Syntaxof 'saved-props+srcloc)
                   First-Comments))
  (p? v
      (make-predicate (Syntaxof 'saved-props+srcloc))
      first-comments?))

(: first-comments2? (→ Any Boolean : (Listof ISyntax)))
(define (first-comments2? v)
  (and (list? v)
       (andmap isyntax? v)))

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
  (if (first-comments? c)
      c
      #f))

(: extract-comments-after (-> (Syntaxof Any) (U #f Comments-After)))
(define (extract-comments-after stx)
  (define c (syntax-property stx 'comments-after))
  (and (list? c)
       (andmap isyntax? c)
       c))