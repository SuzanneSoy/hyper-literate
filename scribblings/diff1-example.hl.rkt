#lang hyper-literate #:♦ racket/base
♦;(dotlambda/unhygienic . racket/base)

♦title{Highlighting added, removed and existing parts in literate programs}

♦defmodule[hyper-literate/diff1]

Highly experimental. Contains bugs, API may change in the future.

♦defproc[(init) any/c]{
                       
 For now, the ♦racket[init] function must be called somewhere in documents
 which use ♦racketmodname[hyper-literate/diff1]. It produces helper values
 which must be inserted in the scribble document. Simply adding this to the
 document should be enough:

 ♦codeblock|{
 #lang hyper-literate #:♦ racket/base
 ♦(init)}|}

♦defform[(hlite name pat . body)]{
                                  
 Like ♦racket[chunk], but highlights parts of the ♦racket[body] according to
 the pattern ♦racket[pat].

 The ♦racket[pat] should cover the whole ♦racket[body], which can contain
 multiple expressions. The ♦racket[pat] can use the following symbols:

 ♦itemlist[
 ♦item{♦racket[=] to indicate that the following elements are ``normal'' and
   should not be highlighted in any special way.}
 ♦item{♦racket[/] to indicate that the following elements were already
   existing in previous occurrences of the code (the part is dimmed)}
 ♦item{♦racket[+] to indicate that the following elements are new (highlighted
   in green)}
 ♦item{♦racket[-] to indicate that the following elements are removed
   (highlighted in red). Removed elements are also removed from the actual
   executable source code. If a removed element contains one or more normal, new
   or dimmed elements, these children are spliced in place of the removed
   element.}
 ♦item{Other symbols are placeholders for the elements}]

 In the following example, the ♦racket[1] is highlighted as removed (and will
 not be present in the executable code), the ♦racket[π] is highlighted as
 added, and the rest of the code is dimmed:

 ♦codeblock|{
 #lang hyper-literate #:♦ racket/base
 ♦hlite[<my-code> {/ (def args (_ - _ + _ / . _))}
        (define (foo v)
          (+ 1 π . v))]}|

 It produces the result shown below:}

♦require[hyper-literate/diff1]
♦(init)

♦hlite[<my-code> {/ (def args (_ - _ + _ / . _))}
       (define (foo v)
         (+ 1 π . v))]

♦section{Example}

You can look at the source code of this document to see how this example is
done.

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
       (check-equal? '<ddd> '(0 x x . z))]