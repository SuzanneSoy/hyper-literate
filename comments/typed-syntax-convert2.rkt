#lang typed/racket

(require typed-map
         typed/racket/unsafe
         "typed-syntax-convert.rkt")
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
         ISyntax/Not
         ISyntax/Not-E
         (struct-out NonSyntax)
         ;(struct-out NonSexp) ; already exported in typed-syntax-convert.rkt
         NonSyntaxOf
         NonSexpOf
         any->isyntax
         syntax->isyntax
         any->isyntax-e)

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

(define-type ISyntax/Not (ISyntaxOf (NonSyntaxOf Any) (NonSexpOf Any)))
(define-type ISyntax/Not-E (ISyntaxOf-E (NonSyntaxOf Any) (NonSexpOf Any)))

(define-type ISyntax (ISyntaxOf Nothing Nothing))
(define-type ISyntax-E (ISyntaxOf-E Nothing Nothing))

(: syntax->isyntax (∀ (A B) (→ (Syntaxof Any)
                               (→ Any (Values A (U 'modified 'unmodified #;#f)))
                               (→ Any (Values B (U 'modified 'unmodified #;#f)))
                               (Values (ISyntaxOf A B) ;; TODO: change to (SyntaxOf (ISnyntaxOf-E …))
                                       (U 'modified 'unmodified #;#f)))))
(define (syntax->isyntax stx nstx nsexp)
  (define e (syntax-e stx))
  (define-values (e* status) (any->isyntax-e e nstx nsexp))
  (case status
    [(unmodified)
     (values (unsafe-cast e (ISyntaxOf A B)) 'unmodified)]
    [(modified)
     (values (datum->syntax* stx e* stx stx) 'modified)]
    #;[(#f)
     (values #f #f)]))

(: any->isyntax (∀ (A B) (→ Any
                            (→ Any (Values A (U 'modified 'unmodified #;#f)))
                            (→ Any (Values B (U 'modified 'unmodified #;#f)))
                            (Values (ISyntaxOf A B)
                                    (U 'modified 'unmodified #;#f)))))
(define (any->isyntax e nstx nsexp)
  (if (syntax? e)
      (syntax->isyntax e nstx nsexp)
      (nstx e)))

(: listof-any->listof-isyntax
   (∀ (A B) (→ (Listof Any)
               (→ Any (Values A (U 'modified 'unmodified #;#f)))
               (→ Any (Values B (U 'modified 'unmodified #;#f)))
               (Pairof (Listof (ISyntaxOf A B))
                       (U 'modified 'unmodified #;#f)))))
(define (listof-any->listof-isyntax e nstx nsexp)
  (define e+status*
    (map (λ ([eᵢ : Any])
           (let-values ([(eᵢ* status) (any->isyntax eᵢ nstx nsexp)])
             (cons eᵢ* status)))
         e))
  (define e* (map car e+status*))
  (define status* (map cdr e+status*))
  (cond
    [(andmap (curry eq? 'unmodified) status*)
     (cons (unsafe-cast e (Listof (ISyntaxOf A B))) 'unmodified)]
    #;[(ormap (curry eq? #f) status*)
     (cons #f #f)]
    [else
     (cons e* 'modified)]))

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
                           (→ Any (Values A (U 'modified 'unmodified #;#f)))
                           (→ Any (Values B (U 'modified 'unmodified #;#f)))
                           (Values (Pairof (ISyntaxOf A B)
                                           (Rec L (U (ISyntaxOf A B)
                                                     Null
                                                     (Pairof (ISyntaxOf A B)
                                                             L))))
                                   (U 'unmodified 'modified)))))
(define (handle-pair e nstx nsexp)
  (let-values ([(car* status-car)
                 (any->isyntax (car e) nstx nsexp)])
    (cond
      [(pair? (cdr e))
       (let-values ([(cdr* status-cdr)
                     (handle-pair (cdr e) nstx nsexp)])
         (cond
           #;[(and (eq? status-car 'unmodified)
                   (eq? status-cdr 'unmodified))
              (values (unsafe-cast e (Pairof ISyntax
                                             (Rec L (U ISyntax
                                                       Null
                                                       (Pairof ISyntax L)))))
                      'unmodified)]
           #;[(or (eq? status-car #f)
                  (eq? status-cdr #f))
              (values #f #f)]
           [else
            (values (cons car* cdr*) 'modified)]))]
      [(null? (cdr e))
       (cond
         #;[(eq? status-car 'unmodified)
            (values (unsafe-cast e (Pairof ISyntax Null)) 'unmodified)]
         #;[(eq? status-car #f)
            (values #f #f)]
         [else
          (values (cons car* (cdr e)) 'modified)])]
      [else
       (let-values ([(cdr* status-cdr)
                     (any->isyntax (cdr e) nstx nsexp)])
         (cond
           #;[(and (eq? status-car 'unmodified)
                   (eq? status-cdr 'unmodified))
              (values (unsafe-cast e (Pairof ISyntax
                                             (Rec L (U ISyntax
                                                       Null
                                                       (Pairof ISyntax L)))))
                      'unmodified)]
           #;[(or (eq? status-car #f)
                  (eq? status-cdr #f))
              (values #f #f)]
           [else
            (values (cons car* cdr*) 'modified)]))])
    #;[(null? e)
     (values e 'unmodified)]
    #;[else
     (any->isyntax e)]))

#;(let*-values ([(car* status-car)
                 (any->isyntax (car e))]
                [(cdr* status-cdr)
                 (any->isyntax (cdr e))])
    (cond
      #;[(and (eq? status-car 'unmodified)
              (eq? status-cdr 'unmodified))
         (values (unsafe-cast e (ISyntax-E A)) 'unmodified)]
      #;[(or (eq? status-car #f)
             (eq? status-cdr #f))
         (values #f #f)]
      [else
       (values (cons car* cdr*) 'modified)]))

(: any->isyntax-e (∀ (A B) (→ Any
                              (→ Any (Values A (U 'modified 'unmodified #;#f)))
                              (→ Any (Values B (U 'modified 'unmodified #;#f)))
                              (Values (ISyntaxOf-E A B)
                                      (U 'modified 'unmodified #;#f)))))
(define (any->isyntax-e e nstx nsexp)
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
    [(box? e)     (let-values ([(u* status) (any->isyntax (unbox e) nstx nsexp)])
                    (case status
                      [(unmodified)
                       ;(if (immutable? e)
                       ;(values (unsafe-cast e (Sexpof A)) 'unmodified)
                       (values (box-immutable u*) 'modified);)
                       ]
                      [(modified)
                       (values (box-immutable u*) 'modified)]
                      #;[(#f)
                         (values #f #f)]))]
    [(pair? e)    (handle-pair e nstx nsexp)]
    [(vector? e)  (match-let ([(cons vs* status)
                               (listof-any->listof-isyntax (vector->list e) nstx nsexp)])
                    (case status
                      [(unmodified)
                       (if (immutable? e)
                           (values (unsafe-cast e (ISyntaxOf-E A B))
                                   'unmodified)
                           (values (apply vector-immutable vs*)
                                   'modified))]
                      [(modified)
                       (values (apply vector-immutable vs*) 'modified)]
                      #;[(#f)
                         (values #f #f)]))]
    [else
     (nsexp e)]))