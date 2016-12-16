#lang racket/base

(require scribble/reader
         racket/port
         racket/syntax
         syntax/strip-context
         "first-line-utils.rkt")

(provide meta-read-inside
         meta-read-syntax-inside)

(define (meta-read-inside in . args)
  (displayln args)
  (apply read-inside args))

(define (meta-read-syntax-inside source-name in . args)
  (with-syntax* ([rd1 (read-syntax-whole-first-line source-name in)]
                 [rd (apply read-syntax-inside source-name in args)])
    #'(rd1 . rd)))