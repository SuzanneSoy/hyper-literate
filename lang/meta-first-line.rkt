#lang racket/base

(require scribble/reader
         racket/port
         racket/syntax
         syntax/stx
         syntax/strip-context
         "first-line-utils.rkt"
         (only-in "../comment-reader.rkt" make-comment-readtable)
         "../comments/hide-comments.rkt")

(provide meta-read-inside
         meta-read-syntax-inside)

(define (make-at-reader+comments #:syntax? [syntax? #t]
                                 #:inside? [inside? #f]
                                 #:char [command-char #\@])
  (make-at-reader
   #:syntax? syntax?
   #:inside? inside?
   #:command-char command-char
   #:datum-readtable (λ (rt)
                       (make-comment-readtable
                        #:readtable rt
                        #:comment-wrapper '#%comment
                        #:unsyntax #f))))

(define (get-command-char rd1)
  (define rd1-datum (if (syntax? rd1) (syntax->datum rd1) rd1))
  (if (and (pair? rd1-datum)
           (keyword? (car rd1-datum))
           (= 1 (string-length (keyword->string (car rd1-datum)))))
      (values (string-ref (keyword->string (car rd1-datum)) 0)
              (if (syntax? rd1)
                  (datum->syntax rd1 (stx-cdr rd1) rd1 rd1)
                  (cdr rd1)))
      (values #\@ rd1)))

(define (meta-read-inside in . args)
  (define rd1 (read-whole-first-line in))
  (define-values (at-exp-char new-rd1) (get-command-char #'rd1))
  (define rd (apply (make-at-reader+comments #:syntax? #f
                                             #:inside? #t
                                             #:char at-exp-char)
                    args))
  `(,new-rd1 . ,rd))

(define (meta-read-syntax-inside source-name in . args)
  (with-syntax ([rd1 (read-syntax-whole-first-line source-name in)])
    (let-values ([(command-char new-rd1) (get-command-char #'rd1)])
      (with-syntax* ([new-rd1-stx new-rd1]
                     [rd (apply (make-at-reader+comments #:syntax? #t
                                                         #:inside? #t
                                                         #:char command-char)
                                source-name
                                in
                                args)]
                     [rd-hide (hide-#%comment #'rd)])
        #'(new-rd1-stx . rd-hide)))))