#lang racket

(require rackunit
         "../../comments/hide-comments.rkt"
         "../../comments/restore-comments.rkt"
         "same-syntax.rkt")

(define round-trip (compose restore-#%comment hide-#%comment))

(define-syntax (check-round-trip stx)
  (syntax-case stx ()
    [(_ a)
     (datum->syntax #'here
                    `(begin
                       (check-same-syntax (round-trip ,#'a) ,#'a)
                       (check-equal? (syntax->datum (round-trip ,#'a))
                                     (syntax->datum ,#'a)))
                    stx)]))

;; =============================================================================

(let ([stx #'(a b c)])
  (check-same-syntax stx (hide-#%comment stx)))

(check-round-trip #'(a (#%comment "b") c))
  
(check-round-trip #'((#%comment "0") (#%comment "1")
                                     a
                                     (#%comment "b")
                                     (#%comment "bb")
                                     c
                                     (#%comment "d")
                                     (#%comment "dd")))
(check-round-trip #'([#%comment c1]
                     a
                     [#%comment c2]
                     . ([#%comment c3] b [#%comment c4])))
(check-round-trip #'([#%comment c1]
                     a
                     [#%comment c2]
                     . ([#%comment c3]
                        . ([#%comment c4] b [#%comment c5]))))
(check-round-trip #'([#%comment c1]
                     a
                     [#%comment c2]
                     . ([#%comment c3]
                        . ([#%comment c4] [#%comment c5]))))
(check-round-trip #'([#%comment c1]
                     a
                     ([#%comment c2])
                     b))
(check-round-trip #'([#%comment c1]
                     a
                     ([#%comment c2] . b)
                     c))