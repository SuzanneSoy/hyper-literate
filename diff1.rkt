#lang at-exp racket/base

(provide hlite)

(require hyper-literate
         (for-syntax syntax/parse
                     (rename-in racket/base [... …])
                     racket/match
                     syntax/srcloc)
         scribble/core
         scribble/html-properties
         scribble/latex-properties
         scribble/base)

;; For debugging.
(define-for-syntax (show-stx e)
  (define (r e)
    (cond
      ([syntax? e]
       (display "#'")
       (r (syntax-e e)))
      [(pair? e)
       (display "(")
       (let loop ([e e])
         (if (pair? e)
             (begin (r (car e))
                    (display " ")
                    (loop (cdr e)))
             (if (null? e)
                 (display ")")
                 (begin
                   (display ". ")
                   (r e)
                   (display ")")))))]
      [else
       (print (syntax->datum (datum->syntax #f e)))]))
  (r e)
  (newline)
  (newline))


(define the-css-addition
  #"
.HyperLiterateNormal {
  filter: initial;
  background: none;
}

.HyperLiterateDim {
  filter: brightness(150%) contrast(30%) opacity(0.7);
  background: none; /* rgba(82, 103, 255, 0.36); */
}

.HyperLiterateAdd{
  filter: initial;
  background: rgb(202, 226, 202);
}

.HyperLiterateRemove {
  filter: initial;
  background: rgb(225, 182, 182);
}")

(define the-latex-addition
  #"
%\\usepackage{framed}% \begin{snugshade}\end{snugshade}
\\definecolor{HyperLiterateDimColor}{RGB}{210,210,210}
\\definecolor{HyperLiterateAddColor}{RGB}{202,226,202}
\\definecolor{HyperLiterateRemoveColor}{RGB}{225,182,182}

\\def\\HyperLiterateNormal#1{#1}
\\def\\HyperLiterateDim#1{\\colorbox{HyperLiterateDimColor}{%
  \\vphantom{\\RktPn{(}\\RktValDef{Xj}}#1}}
\\def\\HyperLiterateAdd#1{\\colorbox{HyperLiterateAddColor}{%
  \\vphantom{\\RktPn{(}\\RktValDef{Xj}}#1}}
\\def\\HyperLiterateRemove#1{\\colorbox{HyperLiterateRemoveColor}{%
  \\vphantom{\\RktPn{(}\\RktValDef{Xj}}#1}}
")

(define (init)
  (elem
   #:style (style #f
                  (list (css-addition the-css-addition)
                        (tex-addition the-latex-addition)))))

(begin-for-syntax
  (define (stx-null? e)
    (or (null? e)
        (and (syntax? e)
             (null? (syntax-e e)))))
  (define (stx-pair? e)
    (or (pair? e)
        (and (syntax? e)
             (pair? (syntax-e e))))))

(define-syntax (hlite stx)
  (syntax-case stx ()
    [(self name guide1 . body)
     (and (identifier? #'self)
          (identifier? #'name))
     (let ()
       (define (simplify-guide g)
         (cond
           [(and (identifier? g) (free-identifier=? g #'/)) '/]
           [(and (identifier? g) (free-identifier=? g #'=)) '=]
           [(and (identifier? g) (free-identifier=? g #'-)) '-]
           [(and (identifier? g) (free-identifier=? g #'+)) '+]
           [(and (identifier? g) (free-identifier=? g #'-/)) '-/]
           [(and (identifier? g) (free-identifier=? g #'-=)) '-=]
           [(and (identifier? g) (free-identifier=? g #'-+)) '-+]
           [(identifier? g) '_]
           [(syntax? g) (simplify-guide (syntax-e g))]
           [(pair? g) (cons (simplify-guide (car g))
                            (simplify-guide (cdr g)))]
           [(null? g) '()]))
       (define (mode→style m)
         (case m
           [(/) "HyperLiterateDim"]
           [(=) "HyperLiterateNormal"]
           [(-) "HyperLiterateRemove"]
           [(+) "HyperLiterateAdd"]
           [(-/) "HyperLiterateDim"]
           [(-=) "HyperLiterateNormal"]
           [(-+) "HyperLiterateAdd"]))
       (define simplified-guide (simplify-guide #'guide1))
       (define (syntax-e? v)
         (if (syntax? v) (syntax-e v) v))
       (define new-body
         (let loop ([mode '=]
                    [guide simplified-guide]
                    [body #'body])
           (match guide
             [(cons (and new-mode (or '/ '= '- '+ '-/ '-= '-+)) rest-guide)
              (loop new-mode rest-guide body)]
             [(list car-guide rest-guide)
              #:when (and (pair? (syntax-e? body))
                          (memq (syntax-e? (car (syntax-e? body)))
                                '[quote quasiquote
                                  unquote unquote-splicing
                                  quasisyntax syntax
                                  unsyntax unsyntax-splicing])
                          (pair? (syntax-e? (cdr (syntax-e? body))))
                          (null? (syntax-e?
                                  (cdr (syntax-e? (cdr (syntax-e? body))))))
                          (let ([sp (syntax-span (car (syntax-e? body)))])
                            (or (= sp 1)
                                (= sp 2))))
              (unless (symbol? car-guide)
                (raise-syntax-error 'hlite
                                    (format
                                     "expected pattern ~a, found identifier"
                                     car-guide)
                                    (datum->syntax #f (car (syntax-e? body)))))
              (define result
                `(,(car (syntax-e? body))
                  ,(loop mode
                         rest-guide
                         (car (syntax-e? (cdr (syntax-e? body)))))))
              (if (syntax? body)
                  (datum->syntax body result body body)
                  body)]
             [(cons car-guide rest-guide)
              (unless (pair? (syntax-e? body))
                (raise-syntax-error 'hlite
                                    (format
                                     "expected pair ~a, found non-pair"
                                     guide)
                                    (datum->syntax #f body)))
              (define loop2-result
                (let loop2 ([first-iteration? #t]
                            [guide guide]
                            [body (if (syntax? body) (syntax-e body) body)]
                            [acc '()])
                  (cond
                    [(and (pair? guide)
                          (memq (car guide) '(/ = - + -/ -= -+)))
                     (if first-iteration?
                         (loop (car guide) (cdr guide) body)
                         ;; produce:
                         ;; ({code:hilite {code:line accumulated ...}} . rest)
                         (let ([r-acc (reverse acc)]
                               [after (loop (car guide) (cdr guide) body)])
                           (define (do after)
                             (datum->syntax
                              (car r-acc)
                              `(code:hilite (code:line ,@r-acc . ,after)
                                            ,(mode→style mode))
                              (build-source-location-list
                               (update-source-location (car r-acc)
                                                       #:span 0))))
                           (if (stx-pair? body)
                               ;; TODO: refactor the two branches, they are very
                               ;; similar.
                               (cons (do '())
                                     after)
                               ;; Special case to handle (a . b) when b and a
                               ;; do not have the same highlighting.
                               ;; This assigns to the dot the highlighting for
                               ;; b, although it would be possible to assign
                               ;; andother highliughting (just change the
                               ;; mode→style below)
                               (let* ([loc1 (build-source-location-list
                                             (update-source-location
                                              (car acc)
                                              #:span 0))]
                                      [loc2 (build-source-location-list
                                             (update-source-location
                                              after
                                              #:column (- (syntax-column after)
                                                          3) ;; spc + dot + spc
                                              #:span 0))])
                                 `(,(do `(,(datum->syntax
                                            #f
                                            `(code:hilite
                                              ,(datum->syntax
                                                #f `(code:line . ,after) loc2)
                                              ,(mode→style (car guide)))
                                            loc1))))))))]
                    [(and (pair? guide) (pair? body))
                     ;; accumulate the first element of body
                     (loop2 #f
                            (cdr guide)
                            (cdr body)
                            (cons (loop mode (car guide) (car body)) acc))]
                    ;; If body is not a pair, then we will treat it as an
                    ;; "improper tail" element, unless it is null?
                    [(null? body)
                     (unless (null? guide)
                       (raise-syntax-error
                        'hlite
                        ;; TODO: thread the syntax version of body, so that
                        ;; we can highlight the error.
                        "Expected non-null body, but found null"
                        stx))
                     ;; produce:
                     ;; ({code:hilite {code:line accumulated ...}})
                     (let* ([r-acc (reverse acc)])
                       `(,(datum->syntax (car r-acc)
                                         `(code:hilite (code:line . ,r-acc)
                                                       ,(mode→style mode))
                                         (build-source-location-list
                                          (update-source-location (car r-acc)
                                                                  #:span 0))))
                       )]
                    [else
                     ;; produce:
                     ;; ({code:hilite
                     ;;   {code:line accumulated ... . improper-tail}})
                     (let* ([new-body (loop mode guide body)]
                            [r-acc+tail (append (reverse acc) new-body)])
                       `(,(datum->syntax
                           (car r-acc+tail)
                           `(code:hilite (code:line . ,r-acc+tail)
                                         ,(mode→style mode))
                           (build-source-location-list
                            (update-source-location (car r-acc+tail)
                                                    #:span 0))))
                       )
                     ])))
              (if (syntax? body)
                  (datum->syntax body loop2-result body body)
                  loop2-result)]
             [(? symbol?)
              (datum->syntax body `(code:hilite (code:line ,body)
                                                ,(mode→style mode))
                             (build-source-location-list
                              (update-source-location body #:span 0)))]
             ['()
              (unless (stx-null? body)
                (raise-syntax-error
                 'hlite
                 ;; TODO: thread the syntax version of body, so that
                 ;; we can highlight the error.
                 (format "Expected null body, but found non-null ~a"
                         (syntax->datum body))
                 stx))
              body])))
       (define new-executable-code
         (let loop ([mode '=]
                    [guide simplified-guide]
                    [body #'body])
           (match guide
             [(cons (and new-mode (or '/ '= '- '+ '-/ '-= '-+)) rest-guide)
              (loop new-mode rest-guide body)]
             [(cons car-guide rest-guide)
              (define (do-append-last-acc last-acc acc)
                ;; When nothing is later added to acc, we can
                ;; simply put r as the last element of the
                ;; reversed acc. This allows r to be an
                ;; improper list.
                ;; do-append-last-acc is called when elements follow
                ;; the current value of last-acc.
                (unless (syntax->list (datum->syntax #f last-acc))
                  (raise-syntax-error
                   'hlite
                   (format
                    (string-append
                     "the removal of elements caused a list with a"
                     "dotted tail to be spliced in a non-final position: ~a")
                    (syntax->datum (datum->syntax #f last-acc)))
                   stx))
                (append (reverse (syntax->list (datum->syntax #f last-acc)))
                        acc))
              (define loop2-result
                (let loop2 ([first-iteration? #t]
                            [guide guide]
                            [body (if (syntax? body) (syntax-e body) body)]
                            [acc '()]
                            [last-acc '()])
                  (cond
                    [(and (pair? guide)
                          (memq (car guide) '(/ = - + -/ -= -+)))
                     (if (or first-iteration?
                             (eq? (car guide) mode))
                         (loop (car guide) (cdr guide) body)
                         (let ([r (loop (car guide) (cdr guide) body)])
                           (if (stx-null? r)
                               ;; produce: (accumulated ... . last-acc)
                               (append (reverse acc) last-acc)
                               ;; produce: (accumulated ... last-acc ... . rest)
                               (let ([r-acc (reverse (do-append-last-acc
                                                      last-acc
                                                      acc))])
                                 (append r-acc r)))))]
                    [(and (pair? guide) (pair? body))
                     ;; accumulate the first element of body, if mode is not '-
                     ;; which means that the element should be removed.
                     (cond
                       [(and (memq mode '(- -/ -= -+))
                             (or (pair? (car body))
                                 (and (syntax? (car body))
                                      (pair? (syntax-e (car body))))))
                        (let ([r (loop mode (car guide) (car body))])
                          (loop2 #f
                                 (cdr guide)
                                 (cdr body)
                                 (do-append-last-acc last-acc acc)
                                 r))]
                       [(memq mode '(- -/ -= -+))
                        (loop2 #f
                               (cdr guide)
                               (cdr body)
                               acc
                               last-acc)]
                       [else
                        (loop2 #f
                               (cdr guide)
                               (cdr body)
                               (do-append-last-acc last-acc acc)
                               (list (loop mode (car guide) (car body))))])]
                    ;; If body is not a pair, then we will treat it as an
                    ;; "improper tail" element, unless it is null?
                    [(null? body)
                     ;; produce:
                     ;; ((accumulated ...))
                     (let* ([r-acc (append (reverse acc) last-acc)])
                       r-acc)]
                    [else
                     ;; produce:
                     ;; (accumulated ... . improper-tail)
                     (let* ([new-body (loop mode guide body)]
                            [r-acc+tail (append
                                         (reverse
                                          (do-append-last-acc last-acc acc))
                                         new-body)])
                       r-acc+tail)])))
              (if (syntax? body)
                  (datum->syntax body loop2-result body body)
                  loop2-result)]
             [(? symbol?)
              body]
             ['()
              body])))
       ;(displayln new-body)
       ;(show-stx new-body)
       #`(begin
           (init)
           #,(datum->syntax
              stx
              `(,(datum->syntax #'here 'chunk #'self)
                #:display-only
                ,#'name
                . ,(syntax-e new-body))
              stx)
           (chunk #:save-as dummy name
                  . #,new-executable-code)))]))

