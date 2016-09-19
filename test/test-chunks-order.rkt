#lang hyper-literate racket/base

@chunk[<values>
       'A]

@chunk[<values>
       'B]

@CHUNK[<values>
       'C]

@CHUNK[<values>
       'D]

@chunk[<values>
       'E]

@chunk[<values>
       'F]

@CHUNK[<values>
       'G]

@CHUNK[<values>
       'H]

@chunk[<*>
       (require rackunit)
       (check-equal? (list <values>)
                     '(A B C D E F G H))]
