#lang racket/base

(require scribble/reader
         racket/port)

(provide meta-read-inside
         meta-read-syntax-inside)

(define (read-line-length port)
  (let* ([peeking (peeking-input-port port)]
         [start (file-position peeking)]
         [_ (read-line peeking)]
         [end (file-position peeking)])
    (- end start)))

(define (narrow-to-one-line port)
  (make-limited-input-port port (read-line-length port)))

(define (meta-read-inside in . args)
  (displayln args)
  (apply read-inside args))

(define (meta-read-syntax-inside source-name in . args)
  (define in1 (narrow-to-one-line in))
  (with-syntax ([rd1 (let loop ([res '()])
                       (define res+ (read-syntax source-name in1))
                       (if (eof-object? res+)
                           (reverse res)
                           (loop (cons res+ res))))]
                [rd (apply read-syntax-inside source-name in args)])
    #'(rd1 . rd)))