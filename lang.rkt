#lang racket/base
;; Forked from scribble-lib/scribble/lp/lang/lang2.rkt
(require "private/common.rkt")

(provide (except-out (all-from-out "private/common.rkt")
                     module-begin/plain
                     module-begin/doc)
         (rename-out [module-begin/doc #%module-begin]))
