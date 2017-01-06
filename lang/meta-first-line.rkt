#lang racket/base

(require scribble/reader
         racket/port
         racket/syntax
         syntax/strip-context
         "first-line-utils.rkt"
         (only-in "../comment-reader.rkt" make-comment-readtable))

(provide meta-read-inside
         meta-read-syntax-inside
         restore-#%comment)

(define (make-at-reader+comments #:syntax? [syntax? #t] #:inside? [inside? #f])
  (make-at-reader
   #:syntax? syntax?
   #:inside? inside?
   #:datum-readtable (λ (rt)
                       (make-comment-readtable
                        #:readtable rt
                        #:comment-wrapper '#%comment))))


(define (meta-read-inside in . args)
  (define rd1 (read-whole-first-line in))
  (define rd (apply (make-at-reader+comments #:syntax? #f #:inside? #t)
                    args))
  `(,rd1 . ,rd))

(begin
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
           (for-syntax (rename-in racket/base [... …])))

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
  
  (define/contract (with-comments e c)
    (-> syntax? (or/c #f comments-after/c) syntax?)
    (if (or (not c) (null? c))
        e
        (syntax-property e 'comments-after c)))

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
          (define new-e* (map with-comments
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
           (with-comments
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

  (define/contract (extract-first-comments stx)
    (-> syntax? (or/c #f first-comments/c))
    (syntax-property stx 'first-comments))

  (define/contract (extract-comments-after stx)
    (-> syntax? (or/c #f comments-after/c))
    (syntax-property stx 'comments-after))

  (define (restore-#%comment stx
                             #:replace-with (replace-with #f)
                             #:scope [scope (datum->syntax #f 'zero)])
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
         (datum->syntax commentᵢ
                        `(,(datum->syntax #'c replace-with #'c #'c)
                          . ,((make-syntax-delta-introducer
                               scope
                               (datum->syntax #f 'zero))
                              #'rest
                              'add))
                        commentᵢ
                        commentᵢ)]
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

  (define (same-syntax! a b)
    (define answer (equal? (annotate-syntax a #:srcloc+scopes? #f)
                           (annotate-syntax b #:srcloc+scopes? #f)))
    (unless answer
      (pretty-write
       (sexp-diff (annotate-syntax a)
                  (annotate-syntax b)))
      (displayln a)
      (displayln b))
    answer)
      
  (define-syntax (check-same-syntax stx)
    (syntax-case stx ()
      [(_ a b)
       (datum->syntax #'here
                      `(check-true (same-syntax! ,#'a ,#'b))
                      stx)]))
  
  (module+ test
    (require rackunit)
    (let ([stx #'(a b c)])
      (check-same-syntax stx (hide-#%comment stx))))
  
  (define round-trip (compose restore-#%comment hide-#%comment))
    
  (module+ test
    (define-syntax (check-round-trip stx)
      (syntax-case stx ()
        [(_ a)
         (datum->syntax #'here
                        `(begin
                           (check-same-syntax (round-trip ,#'a) ,#'a)
                           (check-equal? (syntax->datum (round-trip ,#'a))
                                         (syntax->datum ,#'a)))
                        stx)]))
  
    (check-round-trip #'(a (#%comment "b") c))
  
    (check-round-trip #'((#%comment "0") (#%comment "1")
                                         a
                                         (#%comment "b")
                                         (#%comment "bb")
                                         c
                                         (#%comment "d")
                                         (#%comment "dd")))
    (check-round-trip #'([#%comment c1]
                         a
                         [#%comment c2]
                         . ([#%comment c3] b [#%comment c4])))
    (check-round-trip #'([#%comment c1]
                         a
                         [#%comment c2]
                         . ([#%comment c3]
                            . ([#%comment c4] b [#%comment c5]))))
    (check-round-trip #'([#%comment c1]
                         a
                         [#%comment c2]
                         . ([#%comment c3]
                            . ([#%comment c4] [#%comment c5]))))
    (check-round-trip #'([#%comment c1]
                         a
                         ([#%comment c2])
                         b))
    (check-round-trip #'([#%comment c1]
                         a
                         ([#%comment c2] . b)
                         c)))
  ;; TODO: test restore-comments on an expression which has an 'after-comments
  )

(define (meta-read-syntax-inside source-name in . args)
  (with-syntax* ([rd1 (read-syntax-whole-first-line source-name in)]
                 [rd (apply (make-at-reader+comments #:syntax? #t #:inside? #t)
                            source-name
                            in
                            args)]
                 [rd-hide (hide-#%comment #'rd)])
    #'(rd1 . rd-hide)))