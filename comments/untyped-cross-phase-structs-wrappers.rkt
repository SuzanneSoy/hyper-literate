#lang racket

(require "typed-cross-phase-structs.rkt"
         (for-syntax racket/struct-info))

(define-syntax-rule (define+provide-struct-wrapper-single-field
                      [struct:S make-S S? S-ref S field S-field S-struct-info]
                      ...)
  (begin
    (begin
      (provide (struct-out S))
      (define S-field
        (values (make-struct-field-accessor S-ref 0 'field)))
      (begin-for-syntax
        (struct S-struct-info ()
          #:transparent
          #:property prop:struct-info
          (λ (self)
            (list #'struct:S
                  #'make-S
                  #'S?
                  (list #'S-field) ;; in reverse order
                  (list #f) ;; in reverse order
                  #t))
          #:property prop:set!-transformer
          (lambda (stx)
            (syntax-case stx (set!)
              [(set! id _)
               (raise-syntax-error 'set! "Cannot mutate struct identifier" stx)]
              [id
               (identifier? #'id)
               #'make-S]
              [(id . args)
               (identifier? #'id)
               (syntax/loc stx
                 (make-S . args))]))))
      (define-syntax S (S-struct-info)))
    ...))

(define+provide-struct-wrapper-single-field
  [struct:NonSexp make-NonSexp NonSexp? NonSexp-ref
                  NonSexp v NonSexp-v NonSexp-struct-info]

  [struct:NonSyntax make-NonSyntax NonSyntax? NonSyntax-ref
                    NonSyntax v NonSyntax-v NonSyntax-struct-info]

  [struct:Some make-Some Some? Some-ref
               Some v Some-v Some-struct-info])
