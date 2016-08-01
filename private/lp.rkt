#lang scheme/base
;; Forked from scribble-lib/scribble/private/lp.rkt

(require (for-syntax scheme/base
                     syntax/boundmap
                     syntax/parse
                     racket/syntax)
         scribble/scheme scribble/decode scribble/manual scribble/struct)

(begin-for-syntax
  ;; maps chunk identifiers to a counter, so we can distinguish multiple uses
  ;; of the same name
  (define chunk-numbers (make-free-identifier-mapping))
  (define (get-chunk-number id)
    (free-identifier-mapping-get chunk-numbers id (lambda () #f)))
  (define (inc-chunk-number id)
    (free-identifier-mapping-put!
     chunk-numbers id
     (+ 1 (free-identifier-mapping-get chunk-numbers id))))
  (define (init-chunk-number id)
    (free-identifier-mapping-put! chunk-numbers id 2)))

(require (for-syntax "no-auto-require.rkt"))
(define-for-syntax ((make-chunk racketblock unsyntax?) stx)
  (syntax-parse stx
    [(_ (~optional (~seq #:save-as save-as)) name expr ...)
     ;; no need for more error checking, using chunk for the code will do that
     (identifier? #'name)
     (let* ([n (get-chunk-number (syntax-local-introduce #'name))]
            [str (symbol->string (syntax-e #'name))]
            [tag (format "chunk:~a:~a" str (or n 1))])
     
       (when n
         (inc-chunk-number (syntax-local-introduce #'name)))

       ;; Lift the code so that it is caught by `extract-chunks` in common.rkt
       ;(syntax-local-lift-expression #'(quote-syntax (a-chunk name expr ...)))
       
       ;; Convoluted trick to allow unsyntax in chunks of code. The unsyntax
       ;; escapes the chunk so that code can be injected at compile-time.
       ;; The identifiers inside the escaped portion need to be available both
       ;; for-syntax i.e. (for-meta 1) and (for-meta 0). This is because the
       ;; underlying @racketblock expands the code at run-time, but the
       ;; extract-chunks function in common.rkt looks at the expanded source
       ;; code.
       ;; For now, only #, i.e. unsyntax is supported, within @chunk.
       ;; Later support for UNSYNTAX within @CHUNK may be added.
       (if unsyntax?
           ;; New hack:
           (syntax-local-lift-module-end-declaration
            #'(begin
                (define-syntax (macro-to-expand-unsyntax _)
                  (define a #'here)
                  (define b (syntax-local-identifier-as-binding
                             (syntax-local-introduce #'here)))
                  (define intr (make-syntax-delta-introducer b a))
                  (syntax-local-lift-expression
                   (intr #`(quote-syntax (a-chunk name expr ...)) 'flip))
                  #'(begin))
                (macro-to-expand-unsyntax)))
           ;; Default (old) behaviour, which does not support escaping (via #,):
           (syntax-local-lift-expression
            #'(quote-syntax (a-chunk name expr ...))))

       
       ;; Extract require forms
       (with-syntax ([tag tag]
                     [str str]
                     [((for-label-mod ...) ...)
                      (if (unbox no-auto-require?)
                          #'()
                          (map (lambda (expr)
                                 (syntax-case expr (require)
                                   [(require mod ...)
                                    (let loop ([mods (syntax->list
                                                      #'(mod ...))])
                                      (cond
                                        [(null? mods) null]
                                        [else
                                         (syntax-case (car mods)
                                           (for-syntax quote submod)
                                           [(submod ".." . _)
                                            (loop (cdr mods))]
                                           [(submod "." . _)
                                            (loop (cdr mods))]
                                           [(quote x)
                                            (loop (cdr mods))]
                                           [(for-syntax x ...)
                                            (append (loop (syntax->list
                                                           #'(x ...)))
                                                    (loop (cdr mods)))]
                                           [x
                                            (cons #'x (loop (cdr mods)))])]))]
                                   [else null]))
                               (syntax->list #'(expr ...))))]
                     
                     [(rest ...) (if n
                                     #`((subscript #,(format "~a" n)))
                                     #`())])
         (define/with-syntax pre-content
           #`(make-splice
              (list (make-toc-element
                     #f
                     (list (elemtag '(prefixable tag)
                                    (bold (italic (racket name)) " ::=")))
                     (list (smaller (elemref '(prefixable tag) #:underline? #f
                                             str
                                             rest ...))))
                    (#,racketblock expr ...))))
         #`(begin
             (require (for-label for-label-mod ... ...))
             #,@(if n
                    #'()
                    #'((define-syntax name (make-element-id-transformer
                                            (lambda (stx) #'(chunkref name))))
                       (begin-for-syntax (init-chunk-number #'name))))
             #,(if (attribute save-as)
                   #'(define-syntax (save-as s) (syntax pre-content))
                   #'pre-content))))]))

(define-syntax chunk (make-chunk #'racketblock #t))
(define-syntax CHUNK (make-chunk #'RACKETBLOCK #f))

(define-syntax (chunkref stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([tag (format "chunk:~a:1" (syntax-e #'id))]
                   [str (format "~a" (syntax-e #'id))])
       #'(elemref '(prefixable tag) #:underline? #f str))]))


(provide (all-from-out scheme/base
                       scribble/manual)
         chunk CHUNK)
