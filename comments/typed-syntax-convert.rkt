#lang typed/racket

(require typed-map
         typed/racket/unsafe)

(provide try-any->isexp*
         try-any->isexp
         any->isexp+non-sexp
         (struct-out NonSexp))

(unsafe-require/typed racket/function
                      [[identity unsafe-cast-function] (∀ (A) (→ Any A))])
(unsafe-require/typed racket/base
                      [[datum->syntax datum->syntax*]
                       (∀ (A) (→ (Syntaxof Any)
                                 A
                                 (Syntaxof Any)
                                 (Syntaxof Any)
                                 (Syntaxof A)))])

(define-syntax-rule (unsafe-cast v t)
  ((inst unsafe-cast-function t) v))

(define-type (non-sexp-handler A)
  (→ Any
     (Values (U (Sexpof A) #f)
             (U 'unmodified 'modified #f))))

(: try-listof-any->isexp* (∀ (A) (→ (Listof Any)
                                    (non-sexp-handler A)
                                    (U (Pairof (Listof (Sexpof A))
                                               (U 'unmodified 'modified))
                                       (Pairof #f #f)))))

(define (try-listof-any->isexp* e non-sexp)
  (define e+status*
    (map (λ ([eᵢ : Any])
           (let-values ([(eᵢ* status) (try-any->isexp* eᵢ non-sexp)])
             (cons eᵢ* status)))
         e))
  (define e* (map car e+status*))
  (define status* (map cdr e+status*))
  (cond
    [(andmap (curry eq? 'unmodified) status*)
     (cons (unsafe-cast e (Listof (Sexpof A))) 'unmodified)]
    [(ormap (curry eq? #f) status*)
     (cons #f #f)]
    [else
     (cons e* 'modified)]))

(: try-any->isexp* (∀ (A) (→ Any
                             (non-sexp-handler A)
                             (Values (U (Sexpof A) #f)
                                     (U 'unmodified 'modified #f)))))
(define (try-any->isexp* e non-sexp)
  (cond
    [(boolean? e) (values e 'unmodified)]
    [(char? e)    (values e 'unmodified)]
    [(number? e)  (values e 'unmodified)]
    [(keyword? e) (values e 'unmodified)]
    [(null? e)    (values e 'unmodified)]
    [(string? e)  (if (immutable? e)
                      (values e 'unmodified)
                      (values (string->immutable-string e) 'modified))]
    [(symbol? e)  (values e 'unmodified)]
    [(box? e)     (let*-values ([(u) (unbox e)]
                                [(u* status) (try-any->isexp* e non-sexp)])
                    (case status
                      [(unmodified)
                       (if (immutable? e)
                           (values (unsafe-cast e (Sexpof A)) 'unmodified)
                           (values (box-immutable u*) 'modified))]
                      [(modified)
                       (values (box-immutable u*) 'modified)]
                      [(#f)
                       (values #f #f)]))]
    [(pair? e)    (let*-values ([(car* status-car)
                                 (try-any->isexp* (car e) non-sexp)]
                                [(cdr* status-cdr)
                                 (try-any->isexp* (cdr e) non-sexp)])
                    (cond
                      [(and (eq? status-car 'unmodified)
                            (eq? status-cdr 'unmodified))
                       (values (unsafe-cast e (Sexpof A)) 'unmodified)]
                      [(or (eq? status-car #f)
                           (eq? status-cdr #f))
                       (values #f #f)]
                      [else
                       (values (cons car* cdr*) 'modified)]))]
    [(vector? e)  (match-let ([(cons vs* status)
                               (try-listof-any->isexp* (vector->list e) non-sexp)])
                    (case status
                      [(unmodified)
                       (if (immutable? e)
                           (values (unsafe-cast e (Sexpof A)) 'unmodified)
                           (values (apply vector-immutable vs*) 'modified))]
                      [(modified)
                       (values (apply vector-immutable vs*) 'modified)]
                      [(#f)
                       (values #f #f)]))]
    [else
     (non-sexp e)]))


;; Sexp:  

(struct (A) NonSexp ([value : A]) #:type-name NonSexpOf)

(: any->isexp+non-sexp (→ Any (Sexpof (NonSexpOf Any))))
(define (any->isexp+non-sexp e)
  (let*-values ([(e* status) (try-any->isexp*
                              e
                              (λ (non-sexp-e)
                                (values (NonSexp non-sexp-e)
                                        'modified)))])
    (case status
      [(unmodified) (unsafe-cast e (Sexpof (NonSexpOf Any)))]
      [(modified) e*]
      [(#f)
       (error
        (string-append "Got #f from try->any-isexp* using non-sexp which does"
                       " not return #f."))])))


(: try-any->isexp (→ Any (U (List Any) #f)))
(define (try-any->isexp e)
  (let*-values ([(e* status) (try-any->isexp*
                              e
                              (λ (non-sexp-e)
                                (values #f #f)))])
    (case status
      [(unmodified) (list e)]
      [(modified) (list e*)]
      [(#f) #f])))
