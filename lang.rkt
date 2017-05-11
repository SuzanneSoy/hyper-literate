#lang racket/base
;; Forked from scribble-lib/scribble/lp/lang/lang2.rkt
(require "private/common.rkt")

(provide (rename-out [module-begin/doc #%module-begin])
         ;; TODO: this is the #%top-interaction from racket/base, not from the
         ;; user-specified language.
         #;#%top-interaction)
