#lang hyper-literate racket/base

@chunk[<values>
       'A]

@chunk[<values>
       'B]

@CHUNK[<values>
       'C]

@CHUNK[<values>
       'D]


@chunk[<*>
       (require rackunit)
       (check-equal? (list <values>)
                     '(A B C D))]
