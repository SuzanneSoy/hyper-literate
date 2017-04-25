#lang s-exp syntax/module-reader
;; Forked from scribble-lib/scribble/lp/lang/reader.rkt

hyper-literate/lang

#:read meta-read-inside
#:read-syntax meta-read-syntax-inside
#:whole-body-readers? #t
;; don't use scribble-base-info for the #:info arg, since
;; scribble/lp files are not directly scribble'able.
#:language-info (scribble-base-language-info)
#:info (wrapped-scribble-base-reader-info)
(require "meta-first-line.rkt"
         (only-in scribble/base/reader
                  scribble-base-reader-info
                  scribble-base-language-info)
         "first-line-utils.rkt")

(define orig-scribble-base-reader-info
  (scribble-base-reader-info))

(require syntax-color/scribble-lexer
         syntax-color/racket-lexer
         racket/port)

(define (wrapped-scribble-base-reader-info)
  (define (read/at-exp in offset x-mode)
    (define-values (mode2 lexr command-char mode)
      (apply values x-mode))

    (define-values (r1 r2 r3 r4 r5 max-back-up new-mode)
      (lexr in offset mode))
    (define new-x-mode (list 'main lexr command-char new-mode))

    (values r1 r2 r3 r4 r5 max-back-up new-x-mode))

  (define (make-lexr command-char)
    (make-scribble-inside-lexer #:command-char (or command-char #\@)))
  
  (define (read/options in offset x-mode)
    (define-values (line column position) (port-next-location in))
    (define-values (mode2 command-char)
      (apply values x-mode))

    (define-values (txt type paren start end) (racket-lexer in))
    ;; TODO: limit the number of newlines to a single newline.
    (if (and (eq? type 'white-space)
             (regexp-match #px"\n" txt))
        (values txt type paren start end
                0 (list 'main (make-lexr command-char) command-char #f))
        (let ()
          (define new-command-char
            (or command-char
                (if (memq type '(comment sexp-comment white-space))
                    #f
                    (if (eq? type 'hash-colon-keyword)
                        (let ([rd (read (open-input-string txt))])
                          (if (and (keyword? rd)
                                   (= (string-length (keyword->string rd)) 1))
                              (string-ref (keyword->string rd) 0)
                              #\@))
                        #\@))))
          (values txt type paren start end
                  0 (list 'options new-command-char)))))
  
  (lambda (key defval default)
    (case key
      [(color-lexer)
       (λ (in offset x-mode)
         (define-values (line column position) (port-next-location in))
         (cond
           [(eq? x-mode #f)
            (read/options in offset (list 'options #f))]
           [(eq? (car x-mode) 'options)
            (read/options in offset x-mode)]
           [else
            (read/at-exp in offset x-mode)]))]
      [else
       (orig-scribble-base-reader-info key defval default)])))
