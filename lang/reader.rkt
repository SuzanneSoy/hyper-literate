#lang s-exp syntax/module-reader
;; Forked from scribble-lib/scribble/lp/lang/reader.rkt

hyper-literate/lang

#:read meta-read-inside
#:read-syntax meta-read-syntax-inside
#:whole-body-readers? #t
;; don't use scribble-base-info for the #:info arg, since
;; scribble/lp files are not directly scribble'able.
#:language-info (scribble-base-language-info)
#:info (scribble-base-reader-info)
(require "meta-first-line.rkt"
         (only-in scribble/base/reader
                  scribble-base-reader-info
                  scribble-base-language-info))
