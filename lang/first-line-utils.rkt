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
  (define in1 (peeking-input-port (narrow-to-one-line in)))
  
  (define start-pos (file-position in1))

  (let loop ([last-good-pos start-pos])
    (define res+
      ;; Try to read (may fail if the last object to read spills onto the next
      ;; lines. We read from the peeking-input-port, so that we can retry the
      ;; last read on the full, non-narrowed port.
      (with-handlers ([exn:fail:read? (λ (_) 'read-error)])
        (list (rec-read in1))))
    (cond
      [(eq? res+ 'read-error)
       ;; Last read was unsuccessful, only consume the bytes from the original
       ;; input port up to the last successful read. Then, re-try one last read
       ;; on the whole file (i.e. the last read object may span several lines).
       (read-bytes (- last-good-pos start-pos) in)
       (list (rec-read in))]
      [(eof-object? (car res+))
       ;; Last successful read, actually consume the bytes from the original
       ;; input port. Technically, last-good-pos and (file-position pk) should
       ;; be the same, since the last read returned #<eof> (and therefore did
       ;; not advance the read pointer.
       (read-bytes (- (file-position in1) start-pos) in)
       '()]
      [else
       ;; One successful read. Prepend it, and continue reading some more.
       (cons (car res+)
             (loop (file-position in1)))])))

(define (read-whole-first-line in)
  (read-*-whole-first-line (λ (in1) (read in1)) in))

(define (read-syntax-whole-first-line source-name in)
  (read-*-whole-first-line (λ (in1) (read-syntax source-name in1)) in))