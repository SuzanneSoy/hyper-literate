#lang racket/base

(provide no-auto-require?)
(define no-auto-require? (box #f))
(provide preexpanding?)
(define preexpanding? (box #f))