#lang hyper-literate/typed typed/racket/base

@(require (for-label typed/racket/base))

@title{Title}

Hello world.

@(if-preexpanding
  (void)
  (require (submod "..")))

@(unless-preexpanding
  (symbol->string ee))

Submodules work:

@chunk[<submod>
       (module ms typed/racket/base
         (define x 1)
         (provide x))

       (module ms2 typed/racket/base
         (define y -1)
         (provide y))]

And can be required:

@chunk[<submod>
       (require 'ms)
       (require (submod "." ms2))]

Submodules with @racket[module*] work too:

@chunk[<submod*>
       (module* ms* racket/base
         (require typed/rackunit)
         <req-dotdot>
         (check-equal? ee 'e123)
         (check-equal? y -1))]

And so does @racket[(require (submod ".." …))]:

@chunk[<req-dotdot>
       (require (submod ".."))
       (require (submod ".." ms2))]

Test with multiple subforms inside require, and coverage for
@racket[for-syntax]:

@chunk[<req-multi>
       (require (for-syntax syntax/stx
                            racket/syntax)
                racket/bool)]

Wrap the @racket[(require (for-syntax racket/base))] in a 
@racket[(begin …)] so that it gets ignored, otherwise
scribble complains some identifiers are loaded twice
for-label, since some identifiers have already been introduced
at meta-level 0 by @racketmodname[typed/racket].

@chunk[<require-for-syntax>
       (begin (require (for-syntax racket/base)))]

@chunk[<*>
       <require-for-syntax>
       (require typed/rackunit)
       <submod>
       <req-multi>
       <submod*>
       (check-true (false? #f));; Should be hyperlinked to the main docs
       (begin-for-syntax
         (define/with-syntax ;; Should be hyperlinked to the main docs
           x
           (stx-car ;; Should be hyperlinked to the main docs
            #'(a . b))))
       (check-equal? (+ x x) 2)
       (check-equal? (+ x y) 0)
       ;; Gives an error because typed/racket/base is used on the #lang line:
       ;curry
       (check-equal? ((make-predicate One) 1) #t)
       (check-equal? (ann 'sym Symbol) 'sym)
       (define (f [x : 'e123]) x)
       (define ee (ann (f 'e123) 'e123))
       (provide ee)]
