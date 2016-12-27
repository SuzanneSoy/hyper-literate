#lang s-exp syntax/module-reader
;; Forked from scribble-lib/scribble/lp/lang/reader.rkt

hyper-literate/lang

#:read meta-read-inside
#:read-syntax meta-read-syntax-inside
#:whole-body-readers? #t
;; don't use scribble-base-info for the #:info arg, since
;; scribble/lp files are not directly scribble'able.
#:language-info (scribble-base-language-info)
#:info orig-scribble-base-reader-info ;(wrapped-scribble-base-reader-info)
(require "meta-first-line.rkt"
         (only-in scribble/base/reader
                  scribble-base-reader-info
                  scribble-base-language-info)
         "first-line-utils.rkt")

(define orig-scribble-base-reader-info
  (scribble-base-reader-info))
 
(define (wrapped-scribble-base-reader-info)
  (lambda (key defval default)
    (case key
      [(color-lexer)
       (let ([lexr (orig-scribble-base-reader-info key defval default)])
         (let ([first? #t])
           (λ (in offset mode)
             (when first?
               (set! first? #f)
               ;; TODO: should return (values "#:opt" 'hash-colon-keyword …) for
               ;; the options
               (read-syntax-whole-first-line (object-name in) in))
             ;; Note that offset and mode are optional
             (lexr in offset mode))))]
      [else
       (orig-scribble-base-reader-info key defval default)])))
