(module typed-cross-phase-structs '#%kernel
  (#%declare #:cross-phase-persistent)

  (#%provide struct:NonSexp make-NonSexp NonSexp? NonSexp-ref)
  (define-values (struct:NonSexp make-NonSexp NonSexp? NonSexp-ref NonSexp-set!)
    (make-struct-type 'NonSexp ;; name
                      #f ;; parent
                      1 ;; arguments to the constructor
                      0 ;; auto-fields
                      #f ;; auto-v
                      '() ;; props
                      #f ;; inspector
                      #f ;; proc-spec
                      (list 0) ;; immutables
                      #f ;; guard
                      'NonSexp ;; constructor-name
                      ))
  
  (#%provide struct:NonSyntax make-NonSyntax NonSyntax? NonSyntax-ref)
  (define-values (struct:NonSyntax make-NonSyntax NonSyntax? NonSyntax-ref NonSyntax-set!)
    (make-struct-type 'NonSyntax ;; name
                      #f ;; parent
                      1 ;; arguments to the constructor
                      0 ;; auto-fields
                      #f ;; auto-v
                      '() ;; props
                      #f ;; inspector
                      #f ;; proc-spec
                      (list 0) ;; immutables
                      #f ;; guard
                      'NonSyntax ;; constructor-name
                      ))

  (#%provide struct:Some make-Some Some? Some-ref)
  (define-values (struct:Some make-Some Some? Some-ref Some-set!)
    (make-struct-type 'Some ;; name
                      #f ;; parent
                      1 ;; arguments to the constructor
                      0 ;; auto-fields
                      #f ;; auto-v
                      '() ;; props
                      #f ;; inspector
                      #f ;; proc-spec
                      (list 0) ;; immutables
                      #f ;; guard
                      'Some ;; constructor-name
                      )))