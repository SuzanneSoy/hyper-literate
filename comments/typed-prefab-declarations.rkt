#lang typed/racket

(struct (A) NonSexp ([v : A]) #:type-name NonSexpOf)
(struct (A) NonSyntax ([v : A]) #:type-name NonSyntaxOf)
(struct (A) Some ([v : A]))
(define-type (Maybe A)
  (U (Some A) #f))

(provide (struct-out NonSexp) NonSexpOf
         (struct-out NonSyntax) NonSyntaxOf
         (struct-out Some)
         Maybe)

(module* test typed/racket
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
          Number)))

  (check-not-exn
   (λ ()
     (ann (let ([n : Any (NonSexp 1)])
            (if (NonSexp? n)
                (if (number? (NonSexp-v n))
                    (NonSexp-v n)
                    2)
                0))
          Number))))