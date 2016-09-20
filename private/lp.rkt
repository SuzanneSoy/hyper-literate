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
    (free-identifier-mapping-put! chunk-numbers id 2))

  (define repeat-chunk-numbers (make-free-identifier-mapping))
  (define (get+increment-repeat-chunk-number! id)
    (let ([current (free-identifier-mapping-get repeat-chunk-numbers
                                                id
                                                (lambda () 1))])
      (free-identifier-mapping-put! repeat-chunk-numbers id (add1 current))
      ;; note: due to multiple expansions, this does not increase exactly one at
      ;; a time but instead it can skip numbers. Since this is not visible by
      ;; the user, and just used as a token in the URL, it's okay as long as
      ;; compiling the same file twice gives the same numbers (which is
      ;; hopefully the case but hasn't been tested).
      current)))

(require (for-syntax "no-auto-require.rkt"))
(define-for-syntax (make-chunk-code unsyntax?)
  (syntax-parser
    ;; no need for more error checking, using chunk for the code will do that
    [(_ name:id expr ...)

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
     (define expand-unsyntax
       (if unsyntax?
           ;; New hack:
           #'((define-syntax (macro-to-expand-unsyntax _)
                (define a #'here)
                (define b (syntax-local-identifier-as-binding
                           (syntax-local-introduce #'here)))
                (define intr (make-syntax-delta-introducer b a))
                (syntax-local-lift-expression
                 (intr #'(quote-syntax (a-chunk ((... ...) name)
                                                ((... ...) expr) ...))
                       'flip))
                #'(void))
              (macro-to-expand-unsyntax))
           ;; Default (old) behaviour, does not support escaping via #,
           (begin (syntax-local-lift-expression
                   #'(quote-syntax (a-chunk name expr ...)))
                  #f)))

     (with-syntax 
         ;; Extract require forms
         ([((for-label-mod ...) ...)
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
                    (syntax->list #'(expr ...))))])
       #`(begin
           #,@(if expand-unsyntax expand-unsyntax #'())
           #,@(if (null? (syntax-e #'(for-label-mod ... ...)))
                  #'()
                  #'((require (for-label for-label-mod ... ...))))))]))
  
(define-for-syntax ((make-chunk-display racketblock) stx)
  (syntax-parse stx
    ;; no need for more error checking, using chunk for the code will do that
    [(_ original-name:id name:id stxn:number expr ...)
     (define n (syntax-e #'stxn))
     (define n-repeat (get+increment-repeat-chunk-number!
                       (syntax-local-introduce
                        (format-id #'name "~a:~a" #'name n))))
     (define str (string-append (symbol->string (syntax-e #'name))))
     (define/with-syntax tag (format "chunk:~a:~a:~a" str n n-repeat))
     (define/with-syntax (rest ...)
       ;; if the would-be-next number for this chunk name is "2", then there is
       ;; only one chunk, whose number is "1". Otherwise, if the number is 3 or
       ;; more, it means that the chunk with number "2" exists, so we should
       ;; display the subscript numbers.
       (if (let ([c (get-chunk-number #'original-name)])
             (and c (> c 2)))
           #`((subscript #,(format "~a" n)))
           #'()))
     #`(make-splice
        (list (make-toc-element
               #f
               (list (elemtag '(prefixable tag)
                              (bold (italic (elemref '(prefixable tag)
                                                     #:underline? #f
                                                     #,str rest ...))
                                    " ::=")))
               (list (smaller (elemref '(prefixable tag) #:underline? #f
                                       #,str
                                       rest ...))))
              (#,racketblock expr ...)))]))

(define-for-syntax (make-chunk chunk-code chunk-display)
  (syntax-parser
    ;; no need for more error checking, using chunk for the code will do that
    [(_ (~optional (~seq #:save-as save-as:id)) name:id expr ...)
     (define n (get-chunk-number (syntax-local-introduce #'name)))
     (define str (symbol->string (syntax-e #'name)))
     
     (when n
       (inc-chunk-number (syntax-local-introduce #'name)))

     (define/with-syntax stx-n (or n 1))
     (define/with-syntax stx-chunk-code chunk-code)
     (define/with-syntax stx-chunk-display chunk-display)
     
     #`(begin
         (stx-chunk-code name expr ...)
         #,@(if n
                #'()
                #'((define-syntax name (make-element-id-transformer
                                        (lambda (stx) #'(chunkref name))))
                   (begin-for-syntax (init-chunk-number #'name))))
         #,(if (attribute save-as)
               #'(define-syntax (save-as s)
                   (syntax-case s ()
                     [(_)
                      #`(save-as #,(syntax-local-introduce
                                    (quote-syntax name)))]
                     [(_ newname)
                      #`(stx-chunk-display
                          #,(syntax-local-introduce
                             (quote-syntax name))
                         newname
                         stx-n
                         #,@(syntax-local-introduce
                             (quote-syntax (expr ...))))]))
               ;; The (list) here is important, to avoid the code being executed
               ;; multiple times in weird ways, when pre-expanding.
               #`(list (stx-chunk-display name name stx-n expr ...))))]))

(define-syntax chunk-code (make-chunk-code #t))
(define-syntax CHUNK-code (make-chunk-code #f))
(define-syntax chunk-display (make-chunk-display #'racketblock))
(define-syntax CHUNK-display (make-chunk-display #'RACKETBLOCK))
(define-syntax chunk (make-chunk #'chunk-code #'chunk-display))
(define-syntax CHUNK (make-chunk #'CHUNK-code #'CHUNK-display))

(define-syntax (chunkref stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([tag (format "chunk:~a:1:1" (syntax-e #'id))]
                   [str (format "~a" (syntax-e #'id))])
       #'(elemref '(prefixable tag) #:underline? #f str))]))


(provide (all-from-out scheme/base
                       scribble/manual)
         chunk CHUNK)
