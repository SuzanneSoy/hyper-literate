;; Copied and modified from https://github.com/racket/scribble/blob/
;;    31ad440b75b189a2b0838aab011544d44d6b580/
;;    scribble-lib/scribble/comment-reader.rkt
(module comment-reader scheme/base
  (require (only-in racket/port peeking-input-port))

  (provide (rename-out [*read read]
                       [*read-syntax read-syntax])
           make-comment-readtable)

  (define unsyntaxer (make-parameter 'unsyntax))

  (define (*read [inp (current-input-port)])
    (parameterize ([unsyntaxer (read-unsyntaxer inp)]
                   [current-readtable (make-comment-readtable)])
      (read/recursive inp)))

  (define (*read-syntax src [port (current-input-port)])
    (parameterize ([unsyntaxer (read-unsyntaxer port)]
                   [current-readtable (make-comment-readtable)])
      (read-syntax/recursive src port)))
  
  (define (read-unsyntaxer port)
    (let ([p (peeking-input-port port)])
      (if (eq? (read p) '#:escape-id)  
          (begin (read port) (read port))
          'unsyntax)))

  (define (make-comment-readtable #:readtable [rt (current-readtable)]
                                  #:comment-wrapper [comment-wrapper 'code:comment]
                                  #:unsyntax [unsyntax? #t])
    (make-readtable rt
                    #\; 'terminating-macro
                    (case-lambda 
                     [(char port)
                      (do-comment port
                                  (lambda () (read/recursive port #\@))
                                  #:comment-wrapper comment-wrapper
                                  #:unsyntax unsyntax?)]
                     [(char port src line col pos)
                      (let ([v (do-comment port
                                           (lambda () (read-syntax/recursive src port #\@))
                                           #:comment-wrapper comment-wrapper
                                           #:unsyntax unsyntax?)])
                        (let-values ([(eline ecol epos) (port-next-location port)])
                          (datum->syntax
                           #f
                           v
                           (list src line col pos (and pos epos (- epos pos))))))])))

  (define (do-comment port
                      recur
                      #:comment-wrapper [comment-wrapper 'code:comment]
                      #:unsyntax [unsyntax? #t])
    (define comment-text
      `(t
        ,@(append-strings
           (let loop ()
             (let ([c (read-char port)])
               (cond
                 [(or (eof-object? c)
                      (char=? c #\newline))
                  null]
                 [(char=? c #\@)
                  (cons (recur) (loop))]
                 [else 
                  (cons (string c)
                        (loop))]))))))
    (define comment-unsyntax
      (if unsyntax?
          `(,(unsyntaxer) ,comment-text)
          comment-text))
    `(,comment-wrapper ,comment-text))
  
  (define (append-strings l)
    (let loop ([l l][s null])
      (cond
       [(null? l) (if (null? s)
                      null
                      (preserve-space (apply string-append (reverse s))))]
       [(string? (car l))
        (loop (cdr l) (cons (car l) s))]
       [else
        (append (loop null s)
                (cons
                 (car l)
                 (loop (cdr l) null)))])))

  (define (preserve-space s)
    (let ([m (regexp-match-positions #rx"  +" s)])
      (if m
          (append (preserve-space (substring s 0 (caar m)))
                  (list `(hspace ,(- (cdar m) (caar m))))
                  (preserve-space (substring s (cdar m))))
          (list s)))))
