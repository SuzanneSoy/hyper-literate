#lang racket

(provide first-comments/c
         comments-after/c
         with-first-comments
         with-comments-after
         extract-first-comments
         extract-comments-after)

(define first-comments/c
  (flat-rec-contract R (cons/c (or/c #f (cons/c (syntax/c 'saved-props+srcloc)
                                                R)) #| nested |#
                               (listof syntax?) #| comments |#)))
(define comments-after/c
  (listof syntax?))

(define/contract (with-first-comments e c)
    (-> syntax?
        (or/c #f first-comments/c)
        syntax?)
    (if (or (not c) (and (= (length c) 1) (not (first c))))
        e
        (syntax-property e 'first-comments c)))

(define/contract (with-comments-after e c)
    (-> syntax? (or/c #f comments-after/c) syntax?)
    (if (or (not c) (null? c))
        e
        (syntax-property e 'comments-after c)))

(define/contract (extract-first-comments stx)
    (-> syntax? (or/c #f first-comments/c))
    (syntax-property stx 'first-comments))

  (define/contract (extract-comments-after stx)
    (-> syntax? (or/c #f comments-after/c))
    (syntax-property stx 'comments-after))