#lang typed/racket

(require typed/racket/unsafe)
(require "untyped-cross-phase-structs-wrappers.rkt")
(unsafe-require/typed "untyped-cross-phase-structs-wrappers.rkt"
                      [#:struct (A) NonSexp ([v : A]) #:type-name NonSexpOf]
                      [#:struct (A) NonSyntax ([v : A]) #:type-name NonSyntaxOf]
                      [#:struct (A) Some ([v : A])])

;(require typed-racket/base-env/prims-struct)
;(dtsi* (A) NonSexp NonSexpOf ([v : A]) #:maker make-NonSexp)

(provide (struct-out NonSexp))
;(struct (A) NonSexp ([v : A]) #:type-name NonSexpOf #:constructor-name make-NonSexp)

#;(module* test typed/racket
  (require (submod ".."))
  (require typed/rackunit)
  (check-pred procedure? NonSexp)
  (check-pred NonSexp? (ann (ann (NonSexp 1) (NonSexpOf Number)) Any))
  (check-not-exn
   (λ ()
     (ann (let ([n : (NonSexpOf Any) (NonSexp 1)])
            (if (number? (NonSexp-v n))
                (NonSexp-v n)
                0))
          Number))))