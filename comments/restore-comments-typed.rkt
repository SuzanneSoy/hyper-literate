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

(provide restore-#%comment)

(define/contract (restore-#%comment stx
                                      #:replace-with (replace-with #f)
                                      #:scope [scope (datum->syntax #f 'zero)])
    (->* (syntax?)
         (#:replace-with [or/c #f syntax? (-> syntax? syntax?)]
          #:scope identifier?)
         syntax?)
    (define (erase-props stx)
      (define stx* (if (syntax-property stx 'first-comments)
                       (syntax-property stx 'first-comments #f)
                       stx))
      (if (syntax-property stx* 'comments-after)
          (syntax-property stx* 'comments-after #f)
          stx*))
    (define (recur stx)
      (restore-#%comment stx #:replace-with replace-with #:scope scope))
    (define (replace-in commentᵢ)
      (syntax-parse commentᵢ
        #:datum-literals (#%comment)
        [({~and c #%comment} . rest)
         (if (syntax? replace-with)
             (datum->syntax commentᵢ
                            `(,(datum->syntax #'c replace-with #'c #'c)
                              . ,((make-syntax-delta-introducer
                                   scope
                                   (datum->syntax #f 'zero))
                                  #'rest
                                  'add))
                            commentᵢ
                            commentᵢ)
             (replace-with
              (datum->syntax commentᵢ
                             `(,#'c
                               . ,((make-syntax-delta-introducer
                                    scope
                                    (datum->syntax #f 'zero))
                                   #'rest
                                   'add))
                             commentᵢ
                             commentᵢ)))]
        [_
         commentᵢ]))
    (define (replace-in-after comments)
      (if replace-with
          (if (eq? comments #f)
              comments
              (stx-map replace-in comments))
          comments))
    (define (replace-in-first first-comments)
      (define (replace-in-first1 first-comments)
        (if (eq? first-comments #f)
            first-comments
            (cons (cons (caar first-comments)
                        (replace-in-first1 (cdar first-comments)))
                  (stx-map replace-in (cdr first-comments)))))
      (if replace-with
          (if (eq? first-comments #f)
              first-comments
              (cons (replace-in-first1 (car first-comments))
                    (stx-map replace-in (cdr first-comments))))
          first-comments))
    (match (syntax-e stx)
      [(list* e* ... rest)
       ;; TODO: when extracting the comments properties, check that they have
       ;; the right shape (listof syntax?) or (*list/c syntax? (list/c R))
       ;; Or append-map when stx is a stx-list (not in a tail position for the
       ;;                                       comments-after)
       (define new-e*
         (append-map (λ (eᵢ)
                       (cons (recur eᵢ)
                             (or (replace-in-after (extract-comments-after eᵢ))
                                 '())))
                     e*))
       (define new-rest
         (if (syntax? rest)
             (recur rest)
             ;; TODO: handle vectors etc. here?
             rest))
       (define first-comments
         (or (replace-in-first (extract-first-comments stx))
             #f))
       (define (nest first-comments to-nest)
         (cond
           [(eq? first-comments #f)
            to-nest]
           [(eq? (car first-comments) #f)
            (append (cdr first-comments) to-nest)]
           [else
            (nest1 first-comments to-nest)]))
       (define (nest1 first-comments to-nest)
         (if (eq? first-comments #f)
             to-nest
             (append (cdr first-comments)
                     (datum->syntax (caar first-comments)
                                    (nest (cdar first-comments) to-nest)))))
       (define new-stx
         (nest first-comments (append new-e* new-rest)))
       (erase-props (datum->syntax stx new-stx stx stx))]
      ;; TODO: recurse down vectors etc.
      [(? vector? v)
       ;; TODO: what if there is a first-comment property on the vector itself?
       (erase-props
        (datum->syntax stx
                       (vector-map (λ (vᵢ)
                                     (recur vᵢ))
                                   v)
                       stx
                       stx))]
      [other
       'TODO…
       other]))