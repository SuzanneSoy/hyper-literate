#lang racket/base

(require racket/port)

(provide read-syntax-whole-first-line)

(define (read-line-length port)
  (let* ([peeking (peeking-input-port port)]
         [start (file-position peeking)]
         [_ (read-line peeking)]
         [end (file-position peeking)])
    (- end start)))

(define (narrow-to-one-line port)
  (make-limited-input-port port (read-line-length port)))

(define (read-syntax-whole-first-line source-name in)
  (define in1 (narrow-to-one-line in))
  (let loop ([res '()])
    (define res+ (read-syntax source-name in1))
    (if (eof-object? res+)
        (reverse res)
        (loop (cons res+ res)))))