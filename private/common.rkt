#lang racket/base
;; Forked from scribble-lib/scribble/lp/lang/common.rkt

(provide (except-out (all-from-out racket/base) #%module-begin)
         module-begin/plain
         module-begin/doc)

(require (for-syntax racket/base syntax/boundmap racket/list
                     syntax/strip-context
                     syntax/srcloc
                     racket/struct
                     syntax/srcloc
                     debug-scopes/named-scopes/exptime))

(begin-for-syntax
  (define first-id #f)
  (define main-id #f)
  (define (mapping-get mapping id)
    (free-identifier-mapping-get mapping id (lambda () '())))
  ;; maps a chunk identifier to its collected expressions
  (define chunks (make-free-identifier-mapping))
  ;; maps a chunk identifier to all identifiers that are used to define it
  (define chunk-groups (make-free-identifier-mapping))
  (define (get-chunk id) (mapping-get chunks id))
  (define (add-to-chunk! id exprs)
    (unless first-id (set! first-id id))
    (when (eq? (syntax-e id) '<*>) (set! main-id id))
    (free-identifier-mapping-put!
     chunk-groups id
     (cons id (mapping-get chunk-groups id)))
    (free-identifier-mapping-put!
     chunks id
     `(,@(mapping-get chunks id) ,@exprs))))

(define-for-syntax (tangle orig-stx)
  (define chunk-mentions '())
  (unless first-id
    (raise-syntax-error 'scribble/lp "no chunks"))
  (define (restore nstx d) (datum->syntax orig-stx d nstx nstx))
  (define (shift nstx) (replace-context orig-stx nstx))
  (define body
    (let ([main-id (or main-id first-id)])
      (restore
       main-id
       (let loop ([block (get-chunk main-id)])
         (append-map
          (lambda (expr)
            (if (identifier? expr)
                (let ([subs (get-chunk expr)])
                  (if (pair? subs)
                      (begin (set! chunk-mentions (cons expr chunk-mentions))
                             (loop subs))
                      (list (shift expr))))
                (let ([subs (syntax->list expr)])
                  (if subs
                      (list (restore expr (loop subs)))
                      (list (shift expr))))))
          block)))))
  (with-syntax ([(body0 body ...) (strip-comments body)]
                ;; construct arrows manually
                [((b-use b-id) ...)
                 (append-map (lambda (m)
                               (map (lambda (u)
                                      (list (syntax-local-introduce m) 
                                            (syntax-local-introduce u)))
                                    (mapping-get chunk-groups m)))
                             chunk-mentions)])
    ;; TODO: use disappeared-use and disappeared-binding.
    ;; TODO: fix srcloc (already fixed?).
    ;#`(#,(datum->syntax #'body0 'begin) (let ([b-id (void)]) b-use) ... body0 body ...)
    (syntax-property
     (syntax-property #`(#,(datum->syntax #'body0 'begin) body0 body ...)
                      'disappeared-binding (syntax->list (syntax-local-introduce #'(b-id ...))))
     'disappeared-use (syntax->list (syntax-local-introduce #'(b-use ...))))))

(define-for-syntax (strip-comments body)
  (cond
    [(syntax? body)
     (define r (strip-comments (syntax-e body)))
     (if (eq? r (syntax-e body))
         body
         (datum->syntax body r body body))]
    [(pair? body)
     (define a (car body))
     (define ad (syntax-e a))
     (cond
       [(and (pair? ad)
             (memq (syntax-e (car ad))
                   '(code:comment
                     code:contract)))
        (strip-comments (cdr body))]
       [(eq? ad 'code:blank)
        (strip-comments (cdr body))]
       [(and (or (eq? ad 'code:hilite)
                 (eq? ad 'code:quote))
             (let* ([d (cdr body)]
                    [dd (if (syntax? d)
                            (syntax-e d)
                            d)])
               (and (pair? dd)
                    (or (null? (cdr dd))
                        (and (syntax? (cdr dd))
                             (null? (syntax-e (cdr dd))))))))
        (define d (cdr body))
        (define r
          (strip-comments (car (if (syntax? d) (syntax-e d) d))))
        (if (eq? ad 'code:quote)
            `(quote ,r)
            r)]
       [(and (pair? ad)
             (eq? (syntax-e (car ad))
                  'code:line))
        (strip-comments (append (cdr ad) (cdr body)))]
       [else (cons (strip-comments a)
                   (strip-comments (cdr body)))])]
    [else body]))

(define-for-syntax (extract-chunks exprs)
  (let loop ([exprs exprs])
    (syntax-case exprs ()
      [() (void)]
      [(expr . exprs)
       (syntax-case #'expr (define-values quote-syntax)
         [(define-values (lifted) (quote-syntax (a-chunk id body ...)))
          (eq? (syntax-e #'a-chunk) 'a-chunk)
          (begin
            (add-to-chunk! #'id (syntax->list #'(body ...)))
            (loop #'exprs))]
         [_
          (loop #'exprs)])])))

(require (for-syntax racket/syntax
                     syntax/parse))

(require (for-syntax racket/pretty
                     "no-auto-require.rkt"))

(define-for-syntax (strip-source e)
  (cond [(syntax? e)
         (update-source-location
          (datum->syntax e (strip-source (syntax-e e)) e e)
          #:source #f)]
        [(pair? e) (cons (strip-source (car e)) (strip-source (cdr e)))]
        [(vector? e) (list->vector (strip-source (vector->list e)))]
        [(prefab-struct-key e)
         => (λ (k) (make-prefab-struct k (strip-source (struct->list e))))]
        ;; TODO: hash tables
        [else e]))

;; Many thanks to Alex Knauth and Matthew Flatt for finding out how to make
;; module meta-languages.
(define-syntax (continue stx)
  (syntax-case stx ()
    [(_self lang-module-begin maybe-chain₊ . body)
     (let ()
       (define ch₊ (syntax->list #'maybe-chain₊))
       (define expanded (local-expand 
                         (datum->syntax stx
                                        `(,#'lang-module-begin ,@ch₊ . ,#'body)
                                        stx
                                        stx)
                         'module-begin 
                         (list)))
       (define meta-language-nesting
         ;; Use a module-like scope here, instead of (make-syntax-introducer),
         ;; otherwise DrRacket stops drawing some arrows (why?).
         (make-module-like-named-scope 'meta-language-nesting))
       (syntax-case expanded (#%plain-module-begin)
         [(#%plain-module-begin . expanded-body)
          #`(begin 
              . 
              #,(meta-language-nesting #'expanded-body))]))]))

(define-for-syntax ((make-module-begin submod?) stx)
  (syntax-parse stx
    ;; #:no-require-lang is ignored, but still allowed for compatibility.
    ;; TODO: semantically, the no-require-lang and no-auto-require should be
    ;; before the lang, as they are arguments to hyper-literate itself.
    [(_modbeg {~or (lang:id
                    {~optional (~and no-require-lang #:no-require-lang)}
                    {~optional (~and no-auto-require #:no-auto-require)})
                   ({~optional (~and no-auto-require #:no-auto-require)}
                    (lang:id
                     . chain₊))}
              body0 . body)
     (let ()
       (define lang-sym (syntax-e #'lang))
       (let ([expanded
              (expand `(,#'module
                        scribble-lp-tmp-name hyper-literate/private/lp
                        (require hyper-literate/private/chunks-toc-prefix
                                 (for-syntax racket/base
                                             hyper-literate/private/no-auto-require))
                        (begin-for-syntax (set-box! no-auto-require?
                                                    ,(if (attribute no-auto-require) #t #f))
                                          (set-box! preexpanding? #t))
                        (define-syntax-rule (if-preexpanding a b) a)
                        (define-syntax-rule (when-preexpanding . b) (begin . b))
                        (define-syntax-rule (unless-preexpanding . b) (begin))
                        ,@(strip-context #'(body0 . body))))])
         (syntax-case expanded ()
           [(module name elang (mb . stuff))
            (let ()
              (extract-chunks #'stuff)
              (define/with-syntax tngl
                (tangle #'body0))
              (define/with-syntax mb9 (datum->syntax #f '#%module-begin))
              (define/with-syntax lang-modbeg (datum->syntax #'lang '#%module-begin))
              ; See http://stackoverflow.com/questions/37867859/module-meta-language-in-racket :
              #;(define expanded-main-mod-stx
                  (local-expand
                   (syntax-local-introduce
                    (datum->syntax #f `(,#'module ignored ,(datum->syntax #f lang-sym #'lang #'lang) (,#'mb9 ,(syntax-local-introduce #'tngl)))))
                   'top-level
                   (list)))
              ;(syntax-case expanded-main-mod-stx ();(module #%plain-module-begin)
              ;[(module _ lng11 (#%plain-module-begin . mod-body11))
              #`(#%plain-module-begin
                 #,@(if submod?
                        (list
                         (with-syntax*
                             ([ctx #'body0 #;(syntax-local-introduce #'body0)]
                              ;; TODO: this is a hack, it would be nice to get
                              ;; the actual source location of the lang.
                              [bd1 (update-source-location #'body0
                                                           #:line #f
                                                           #:column #f
                                                           #:position 7
                                                           #:span 14)]
                              [lng (datum->syntax #'ctx 'scribble/doclang2 #'bd1 #'bd1)]
                              [begn (datum->syntax #'ctx 'begin)])
                           (strip-source
                            #`(module* doc lng ;module doc scribble/doclang2
                                #,@(syntax-local-introduce
                                    ;; TODO: instead use:
                                    ;; (begin-for-syntax (set! preexpanding #f))
                                    ;; and make these identifiers exported by
                                    ;; hyper-literate
                                    (strip-context
                                     #`((require hyper-literate/private/chunks-toc-prefix
                                                 (for-syntax racket/base
                                                             hyper-literate/private/no-auto-require))
                                        (begin-for-syntax
                                          (set-box! no-auto-require?
                                                    #,(if (attribute no-auto-require) #t #f))
                                          (set-box! preexpanding? #f))
                                        (define-syntax-rule (if-preexpanding a b)
                                          b)
                                        (define-syntax-rule (when-preexpanding . b)
                                          (begin))
                                        (define-syntax-rule (unless-preexpanding . b)
                                          (begin . b))
                                        (require scribble-enhanced/with-manual
                                                 hyper-literate))))
                                (begn body0 . body)))))
                        '())
                 (require lang)
                 (continue lang-modbeg
                           #,(if (attribute chain₊)
                                 #'(chain₊)
                                 #'())
                           tngl)) ;; TODO: put . tngl and remove the (begin _)
              )])))]))

(define-syntax module-begin/plain (make-module-begin #f))
(define-syntax module-begin/doc (make-module-begin #t))
