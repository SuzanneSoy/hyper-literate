#lang racket/base

(require racket/port)

(provide read-whole-first-line
         read-syntax-whole-first-line
         narrow-to-one-line
         read-line-length)

(define (read-line-length port)
  (let* ([peeking (peeking-input-port port)]
         [start (file-position peeking)]
         [_ (read-line peeking)]
         [end (file-position peeking)])
    (- end start)))

(define (narrow-to-one-line port)
  (make-limited-input-port port (read-line-length port)))

(define (read-*-whole-first-line rec-read in)
  (define in1 (narrow-to-one-line in))
  (let loop ([res '()])
    (define res+ (rec-read in1))
    (if (eof-object? res+)
        (reverse res)
        (loop (cons res+ res)))))

(define (read-whole-first-line in)
  (read-*-whole-first-line (λ (in1) (read in1)) in))

(define (read-syntax-whole-first-line source-name in)
  (read-*-whole-first-line (λ (in1) (read-syntax source-name in1)) in))