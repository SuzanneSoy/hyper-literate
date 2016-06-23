#lang racket/base
;; Forked from scribble-lib/scribble/lp/lang/common.rkt

(provide (except-out (all-from-out racket/base) #%module-begin)
         module-begin/plain
         module-begin/doc)

(require (for-syntax racket/base syntax/boundmap racket/list
                     syntax/strip-context))

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

(define-for-syntax (tangle orig-stx req-lng)
  (define chunk-mentions '())
  (unless first-id
    (raise-syntax-error 'scribble/lp "no chunks"))
  ;(define orig-stx (syntax-case stx () [(_ orig) #'orig]))
  (define (restore nstx d) (datum->syntax orig-stx d nstx nstx))
  (define (shift nstx) (replace-context orig-stx nstx))
  (define body
    (let ([main-id (or main-id first-id)])
      ;; HACK to get arrows drawn for built-ins imported by the module language.
      ;; TODO: it fails with type-expander.lp2.rkt, because it re-requires λ
      ;;       (the new-λ) from 'main.
      (when req-lng
        (free-identifier-mapping-put!
         chunk-groups main-id
         (cons main-id (mapping-get chunk-groups main-id)))
        (free-identifier-mapping-put!
         chunks main-id
         `(,#`(require #,(datum->syntax main-id
                                        req-lng
                                        req-lng
                                        req-lng))
           ,@(mapping-get chunks main-id))))
      ;;;;;;;;;;;;;;
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
  (with-syntax ([(body ...) (strip-comments body)]
                ;; construct arrows manually
                [((b-use b-id) ...)
                 (append-map (lambda (m)
                               (map (lambda (u)
                                      (list (syntax-local-introduce m) 
                                            (syntax-local-introduce u)))
                                    (mapping-get chunk-groups m)))
                             chunk-mentions)])
    ;(displayln (dynamic-require 'tyyyyyyyyyyyped/racket '#%module-begin))
    ;; TODO: use disappeared-use and disappeared-binding.
    ;; TODO: fix srcloc (already fixed?).
    #`(begin (let ([b-id (void)]) b-use) ... body ...)
    #;(replace-context #'#%module-begin;modbeg-ty
                       #`(begin (let ([b-id (void)]) b-use) ... body ...))))

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
       (syntax-case #'expr (define-syntax quote-syntax)
         [(define-values (lifted) (quote-syntax (a-chunk id body ...)))
          (eq? (syntax-e #'a-chunk) 'a-chunk)
          (begin
            (add-to-chunk! #'id (syntax->list #'(body ...)))
            (loop #'exprs))]
         [_ 
          (loop #'exprs)])])))

(require (for-syntax racket/syntax
                     syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require (only-in typed/racket)) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORKAROUND ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require (for-syntax racket/pretty))
(define-for-syntax ((make-module-begin submod?) stx)
  (syntax-parse stx
    [(_modbeg (lang:id (~optional (~and no-require-lang #:no-require-lang)))
              body0 . body)
     (let ()
       (define lang-sym (syntax-e #'lang))
       (let ([expanded 
              (expand `(,#'module
                        scribble-lp-tmp-name hyper-literate/private/lp
                        (define-syntax-rule (if-preexpanding a b) a)
                        (define-syntax-rule (when-preexpanding . b) (begin . b))
                        (define-syntax-rule (unless-preexpanding . b) (begin))
                        ,@(strip-context #'(body0 . body))))])
         (syntax-case expanded ()
           [(module name elang (mb . stuff))
            (let ()
              (extract-chunks #'stuff)
              (dynamic-require lang-sym #f)
              (namespace-require `(for-meta -1 ,lang-sym))
              #;(begin
                  (define/with-syntax tngl (tangle #'body0))
                  (define/with-syntax (tngl0 . tngl*) #'tngl)
                  (define/with-syntax (ex-mod ex-nam ex-lng (ex-#%m . ex-rest))
                    (expand-syntax
                     #`(#,#'module hyper-literate-temp-expand #,lang-sym
                                   #,(replace-context #'here #'tngl))))
                  #`(ex-#%m #,(datum->syntax (syntax-local-introduce #'ex-rest)
                                             '(#%require lang-sym))
                            . ex-rest))
              (define/with-syntax tngl
                (tangle #'body0 (if (attribute no-require-lang) #f #'lang)))
              ;(replace-context
              ;(namespace-symbol->identifier '#%module-begin)
              ;#`(#,(syntax/loc #'lang #%module-begin) …)
              #`(#,(namespace-symbol->identifier '#%module-begin)
                 tngl
                 #,@(maybe-insert-doc-submod submod? lang-sym)))])))]))
(define-for-syntax (maybe-insert-doc-submod submod? lang-sym)
  (if submod?
      (list
       (let ([submod
              (strip-context
               #`(module doc scribble/doclang2
                   (define-syntax-rule (if-preexpanding a b) b)
                   (define-syntax-rule (when-preexpanding . b) (begin))
                   (define-syntax-rule (unless-preexpanding . b) (begin . b))
                   (require scribble/manual
                            (only-in hyper-literate/private/lp
                                     chunk
                                     CHUNK)
                            (for-label #,lang-sym))
                   (begin body0 . body)))])
         (syntax-case submod ()
           [(_ . rest)
            (datum->syntax #'here (cons #'module* #'rest))])))
      '()))

(define-syntax module-begin/plain (make-module-begin #f))
(define-syntax module-begin/doc (make-module-begin #t))
