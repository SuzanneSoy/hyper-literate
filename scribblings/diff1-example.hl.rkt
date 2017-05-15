#lang hyper-literate #:♦ racket/base
♦;(dotlambda/unhygienic . racket/base)

♦title{testttt}

♦require[hyper-literate/diff1]
♦(init)

We define the function foo as follows:

♦chunk[<foo>
       (define (foo v)
         (+ 1 v))]

However, due to implementation details, we need to add ♦racket[π] to this
value:

♦hlite[|<foo'>| {/ (def args (_ _ + _ / . _))}
       (define (foo v)
         (+ 1 π . v))]

In order to optimise the sum of ♦racket[1] and ♦racket[π], we extract the
computation to a global helper constant:


♦hlite[|<foo''>| {+ _ _ / (def args '(+ a - b + c d . e) (_ - _ _ + _ / _)) = _}
       (define π 3.1414592653589793)
       (define one-pus-π (+ 1 π))
       (define (foo v)
         '(a b c d . e)
         (+ 1 π one-pus-π v))0]

♦hlite[|<www>| (/ (quote (+ a - b + c d . e))
                  (quote (+ a - b + c d . e))
                  (= quote (+ a - b + c d . e))
                  (quote (quote (+ a - b + c d . e))))
       '(a b c d . e)
       (quote (a b c d . e))
       (quote (a b c d . e))
       ''(a b c d . e)]

The whole program is therefore:

♦hlite[|<aaa>| {- a + b = c / d}
       1 2 3 4]

♦hlite[<bbb> {- (+ a - b = c)}
       (x y z)]

♦hlite[<ccc> {(z - (+ a - b / . c))}
       (0 (x y . z))]

♦hlite[<ddd> {(z - ((+ a a - b b / . c)))}
       (0 ((x x y yy . z)))]

♦hlite[<eee> {(z - ((+ a a - b b / . c)))}
       (0 ((x x y yy
              . z)))]

♦chunk[<*>
       (require rackunit)
       |<foo''>|
       (check-= (foo 42) (+ 42 1 3.1414592653589793) 0.1)
       (check-equal? (list <www>)
                     '((a c d . e)
                       (a c d . e)
                       (a c d . e)
                       (quote (a c d . e))))
       (check-equal? '(<aaa>) '(2 3 4))
       (check-equal? '(0 <bbb> 1) '(0 x z 1))
       (check-equal? '<ccc> '(0 x . z))
       (check-equal? '<ddd> '(0 x . z))]