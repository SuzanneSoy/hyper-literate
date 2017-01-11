#lang typed/racket

(require typed-map
         typed/racket/unsafe
         "typed-syntax-convert.rkt"
         "maybe.rkt")
(unsafe-require/typed racket/base
                      [[datum->syntax datum->syntax*]
                       (∀ (A) (→ (Syntaxof Any)
                                 A
                                 (Syntaxof Any)
                                 (Syntaxof Any)
                                 (Syntaxof A)))])

(provide ISyntaxOf
         ISyntaxOf-E
         ISyntax
         ISyntax-E
         ISyntax/Non
         ISyntax/Non-E
         (struct-out NonSyntax)
         ;(struct-out NonSexp) ; already exported in typed-syntax-convert.rkt
         NonSyntaxOf
         NonSexpOf
         any->isyntax+non
         syntax->isyntax+non
         any->isyntax-e+non
         try-any->isyntax
         try-syntax->isyntax
         try-any->isyntax-e)

(unsafe-require/typed racket/function
                      [[identity unsafe-cast-function] (∀ (A) (→ Any A))])
(define-syntax-rule (unsafe-cast v t)
  ((inst unsafe-cast-function t) v))

(define-type (ISyntaxOf A B)
  (Rec
   stx
   (U A
      (Syntaxof
       (U B
          Boolean
          Char
          Complex
          Keyword
          String
          Symbol
          (Boxof stx)
          Null
          (Pairof stx (Rec L (U Null
                                stx
                                (Pairof stx L))))
          (Vectorof stx))))))

(define-type (ISyntaxOf-E A B)
  (U B
     Boolean
     Char
     Complex
     Keyword
     String
     Symbol
     (Boxof (ISyntaxOf A B))
     Null
     (Pairof (ISyntaxOf A B) (Rec L (U Null
                                       (ISyntaxOf A B)
                                       (Pairof (ISyntaxOf A B) L))))
     (Vectorof (ISyntaxOf A B))))

(struct (A) NonSyntax ([value : A]) #:type-name NonSyntaxOf)
(struct (A) NonSexp ([value : A]) #:type-name NonSexpOf)

(define-type ISyntax/Non (ISyntaxOf (NonSyntaxOf Any) (NonSexpOf Any)))
(define-type ISyntax/Non-E (ISyntaxOf-E (NonSyntaxOf Any) (NonSexpOf Any)))

(define-type ISyntax (ISyntaxOf Nothing Nothing))
(define-type ISyntax-E (ISyntaxOf-E Nothing Nothing))

(define-type (Result A) (U (Pairof A (U 'modified 'unmodified))
                           (Pairof #f #f)))
(define Result#f (cons #f #f))

(: syntax->isyntax* (∀ (A B) (→ (Syntaxof Any)
                                (→ Any (Result A))
                                (→ Any (Result B))
                                (U (Result (Syntaxof (ISyntaxOf-E A B)))))))
(define (syntax->isyntax* stx nstx nsexp)
  (define e (syntax-e stx))
  (match-define (cons e* status) (any->isyntax-e* e nstx nsexp))
  (case status
    [(unmodified)
     (cons (unsafe-cast e (Syntaxof (ISyntaxOf-E A B))) 'unmodified)]
    [(modified)
     (cons (datum->syntax* stx e* stx stx) 'modified)]
    [(#f)
     Result#f]))

(: any->isyntax* (∀ (A B) (→ Any
                             (→ Any (Result A))
                             (→ Any (Result B))
                             (Result (ISyntaxOf A B)))))
(define (any->isyntax* e nstx nsexp)
  (if (syntax? e)
      (syntax->isyntax* e nstx nsexp)
      (nstx e)))

(: listof-any->listof-isyntax
   (∀ (A B) (→ (Listof Any)
               (→ Any (Result A))
               (→ Any (Result B))
               (Result (Listof (ISyntaxOf A B))))))
(define (listof-any->listof-isyntax e nstx nsexp)
  (define e*+status
    (foldr (λ ([eᵢ : Any] [acc : (Result (Listof (ISyntaxOf A B)))])
             (match-let ([(cons eᵢ* status) (any->isyntax* eᵢ nstx nsexp)])
               (cond
                 [(and (eq? status 'unmodified)
                       (eq? (cdr acc) 'unmodified))
                  (cons (cons eᵢ* (car acc)) 'unmodified)]
                 [(or (eq? status #f)
                      (eq? (cdr acc) #f))
                  Result#f]
                 [else
                  (cons (cons eᵢ* (car acc)) 'modified)])))
           (cons '() 'unmodified)
           e))
  (define e* (car e*+status))
  (define status (cdr e*+status))
  (case status
    [(unmodified) (cons (unsafe-cast e (Listof (ISyntaxOf A B))) 'unmodified)]
    [(modified) (cons e* 'modified)]
    [(#f) Result#f]))

#;(: handle-pair (case→ (→ (Listof Any)
                           (Values (Listof Syntax-E)
                                   (U 'unmodified 'modified)))
                        (→ (Pairof Any (Rec L (U Any (Pairof Any L))))
                           (Values (Pairof Syntax-E
                                           (Rec L (U Syntax-E
                                                     (Pairof Syntax-E L))))
                                   (U 'unmodified 'modified)))
                        (→ Any
                           (Values ISyntax
                                   (U 'unmodified 'modified)))))
#;(: handle-pair (case→ (→ (Pairof Any (Listof Any))
                           (Values (Listof Syntax-E)
                                   (U 'unmodified 'modified)))
                        (→ (Pairof Any (Rec L (U Any (Pairof Any L))))
                           (Values (Pairof Syntax-E
                                           (Rec L (U Syntax-E
                                                     (Pairof Syntax-E L))))
                                   (U 'unmodified 'modified)))))
(: handle-pair (∀ (A B) (→ (U (Pairof Any (Listof Any))
                              (Pairof Any (Rec L (U Any (Pairof Any L)))))
                           (→ Any (Result A))
                           (→ Any (Result B))
                           (Result (Pairof (ISyntaxOf A B)
                                           (Rec L (U (ISyntaxOf A B)
                                                     Null
                                                     (Pairof (ISyntaxOf A B)
                                                             L))))))))
(define (handle-pair e nstx nsexp)
  (define car*+status (any->isyntax* (car e) nstx nsexp))
  (define car* (car car*+status))
  (define status-car (cdr car*+status))
  (cond
    [(pair? (cdr e))
     (match-let ([(cons cdr* status-cdr)
                  (handle-pair (cdr e) nstx nsexp)])
       (cond
         #;[(and (eq? status-car 'unmodified)
                 (eq? status-cdr 'unmodified))
            (cons (unsafe-cast e (Pairof ISyntax
                                         (Rec L (U ISyntax
                                                   Null
                                                   (Pairof ISyntax L)))))
                  'unmodified)]
         [(or (eq? status-car #f)
              (eq? status-cdr #f))
          Result#f]
         [else
          (cons (cons car* cdr*) 'modified)]))]
    [(null? (cdr e))
     (cond
       #;[(eq? status-car 'unmodified)
          (cons (unsafe-cast e (Pairof ISyntax Null)) 'unmodified)]
       [(eq? status-car #f)
        Result#f]
       [else
        (cons (ann (cons car* (cdr e))
                   (Pairof (ISyntaxOf A B)
                           (Rec L (U (ISyntaxOf A B)
                                     Null
                                     (Pairof (ISyntaxOf A B)
                                             L)))))
              'modified)])]
    [else
     (match-let ([(cons cdr* status-cdr) (any->isyntax* (cdr e) nstx nsexp)])
       (cond
         #;[(and (eq? status-car 'unmodified)
                 (eq? status-cdr 'unmodified))
            (cons (unsafe-cast e (Pairof ISyntax
                                         (Rec L (U ISyntax
                                                   Null
                                                   (Pairof ISyntax L)))))
                  'unmodified)]
         [(or (eq? status-car #f)
              (eq? status-cdr #f))
          Result#f]
         [else
          (cons (cons car* cdr*) 'modified)]))]))

(: any->isyntax-e* (∀ (A B) (→ Any
                               (→ Any (Result A))
                               (→ Any (Result B))
                               (Result (ISyntaxOf-E A B)))))
(define (any->isyntax-e* e nstx nsexp)
  (cond
    [(boolean? e) (cons e 'unmodified)]
    [(char? e)    (cons e 'unmodified)]
    [(number? e)  (cons e 'unmodified)]
    [(keyword? e) (cons e 'unmodified)]
    [(null? e)    (cons e 'unmodified)]
    [(string? e)  (if (immutable? e)
                      (cons e 'unmodified)
                      (cons (string->immutable-string e) 'modified))]
    [(symbol? e)  (cons e 'unmodified)]
    [(box? e)     (match-let ([(cons u* status) (any->isyntax* (unbox e) nstx nsexp)])
                    (case status
                      [(unmodified)
                       ;(if (immutable? e)
                       ;(values (unsafe-cast e (Sexpof A)) 'unmodified)
                       (cons (box-immutable u*) 'modified);)
                       ]
                      [(modified)
                       (cons (box-immutable u*) 'modified)]
                      [(#f)
                       Result#f]))]
    [(pair? e)    (handle-pair e nstx nsexp)]
    [(vector? e)  (match-let ([(cons vs* status)
                               (listof-any->listof-isyntax (vector->list e) nstx nsexp)])
                    (case status
                      [(unmodified)
                       (if (immutable? e)
                           (cons (unsafe-cast e (ISyntaxOf-E A B))
                                 'unmodified)
                           (cons (apply vector-immutable vs*)
                                 'modified))]
                      [(modified)
                       (cons (apply vector-immutable vs*) 'modified)]
                      [(#f)
                       Result#f]))]
    [else
     (nsexp e)]))

(: any->isyntax+non (→ Any ISyntax/Non))
(define (any->isyntax+non e)
  (define e*+status
    (any->isyntax* e
                   (λ (n) (cons (NonSyntax n) 'modified))
                   (λ (n) (cons (NonSexp n) 'modified))))
  (if (cdr e*+status)
      (car e*+status)
      (error "Got #f from any->isyntax* with handlers not returning #f")))

(: syntax->isyntax+non (→ (Syntaxof Any) (Syntaxof ISyntax/Non-E)))
(define (syntax->isyntax+non stx)
  (define e*+status
    (syntax->isyntax* stx
                      (λ (n) (cons (NonSyntax n) 'modified))
                      (λ (n) (cons (NonSexp n) 'modified))))
  (if (cdr e*+status)
      (car e*+status)
      (error "Got #f from any->isyntax* with handlers not returning #f")))

(: any->isyntax-e+non (→ Any ISyntax/Non-E))
(define (any->isyntax-e+non e)
  (define e*+status
    (any->isyntax-e* e
                     (λ (n) (cons (NonSyntax n) 'modified))
                     (λ (n) (cons (NonSexp n) 'modified))))
  (if (cdr e*+status)
      (car e*+status)
      (error "Got #f from any->isyntax* with handlers not returning #f")))

(: try-any->isyntax (→ Any (Maybe ISyntax)))
(define (try-any->isyntax e)
  (define e*+status
    ((inst any->isyntax* Nothing Nothing) e
                                          (λ (n) Result#f)
                                          (λ (n) Result#f)))
  (if (cdr e*+status)
      (Some (car e*+status))
      #f))

(: try-syntax->isyntax (→ (Syntaxof Any) (Maybe (Syntaxof ISyntax-E))))
(define (try-syntax->isyntax stx)
  (define e*+status
    ((inst syntax->isyntax* Nothing Nothing) stx
                                             (λ (n) Result#f)
                                             (λ (n) Result#f)))
  (if (cdr e*+status)
      (Some (car e*+status))
      #f))

(: try-any->isyntax-e (→ Any (Maybe ISyntax-E)))
(define (try-any->isyntax-e e)
  (define e*+status
    ((inst any->isyntax-e* Nothing Nothing) e
                                            (λ (n) Result#f)
                                            (λ (n) Result#f)))
  (if (cdr e*+status)
      (Some (car e*+status))
      #f))