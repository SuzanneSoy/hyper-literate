#lang racket/base

(require (for-syntax racket/base
                     racket/syntax)
         (except-in scribble/lp2 chunk CHUNK))

(require (only-in hyper-literate/private/lp
                  chunk
                  CHUNK))

(provide defck
         repeat-chunk
         chunk
         CHUNK)

(define-syntax (defck stx)
  (syntax-case stx ()
    [(self . rest)
     (with-syntax ([(name . content) #'rest]
                   [chk (datum->syntax #'self 'chunk)])
       (with-syntax ([name2 (format-id #'name "~a-repeat" #'name)])
         #`(begin
             #,(syntax/loc stx (chk . rest))
             ;(define name2 #'content)
             (define-syntax (name2 stx2)
               (syntax-case stx2 ()
                 [(_ prefix (... ...)) #'(prefix (... ...) . content)])))))]))

(define-syntax (repeat-chunk stx)
  (syntax-case stx ()
    [(self name)
     (let ([stripped-name (regexp-replace #px"^<(.*)>$"
                                          (symbol->string (syntax-e #'name))
                                          "\\1")])
       (with-syntax ([chk (datum->syntax #'self 'chunk)]
                     [name2 (format-id #'name "~a-repeat" #'name)]
                     [name-rep (format-id #'name "(~a)" stripped-name)])
         #'(name2 chk name-rep)))]))