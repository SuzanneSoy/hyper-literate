#lang scribble/doc
@(require scribble/manual "utils.rkt"
          (for-label scribble/srcdoc scribble/extract racket/contract))

@title[#:tag "srcdoc"]{In-Source Documentation}

The @racketmodname[scribble/srcdoc] and
@racketmodname[scribble/extract] libraries support writing
documentation within the documented code along with an export
contract, similar to using @as-index{JavaDoc}. With this approach, a
single contract specification is used both for the run-time contract
and the documentation of an exported binding.

The @racketmodname[scribble/srcdoc] library provides forms for
exporting a binding with associated documentation. The
@racket[scribble/extract] library is used to pull
@racket[scribble/srcdoc]-based documentation into a Scribble document
(perhaps for multiple libraries).

Although documentation is written with a library's implementation when
using @racketmodname[scribble/srcdoc], the documentation creates no
run-time overhead for the library. Similarly, typesetting the
documentation does not require running the library. The two phases
(run time versus documentation time) are kept separate in much the
same way that the module system keeps expansion-time code separate
from run-time code, and documentation information is recorded in a
submodule to be separately loadable from the enclosing module.

For an example use, see
@hyperlink["https://blog.racket-lang.org/2012/06/submodules.html"]{this post}
at @url{blog.racket-lang.org}.


@; ----------------------------------------

@section{Source Annotations for Documentation}

@defmodule[scribble/srcdoc]

Documentation information generated by @racketmodname[scribble/srcdoc]
forms are accumulated into a @racketidfont{srcdoc} submodule. The
generated submodule is accessed by the bindings of
@racketmodname[scribble/extract].

@defform[(for-doc require-spec ...)]{

A @racket[require] sub-form for bindings that are needed at
documentation time (and documentation-expansion time, etc.) instead of
run time (and expansion time, etc.). A @racket[for-doc] import has
no effect on a normal use of the library; it affects only
documentation extraction.

Typically, a library that uses @racketmodname[scribble/srcdoc]
includes at least @racket[(require (for-doc scribble/base scribble/manual))]
to get core Racket forms and basic Scribble functions to use in
documentation expressions.}

@defform*/subs[#:literals (-> ->* case->)
               [(proc-doc/names id contract arg-specs (desc-expr ...))]
               ([arg-specs ((arg-id ...) ((arg-id default-expr) ...))
                           (arg-id ...)]
                [contract (-> arg ... result)
                          (->* (mandatory ...) (optional ...) result)
                          (case-> (-> arg ... result) ...)]
                [mandatory contract-expr
                           (code:line keyword contract-expr)]
                [optional contract-expr
                          (code:line keyword contract-expr)])]{

A @racket[provide] sub-form that exports @racket[id] with the
contract described by @racket[contract]
just like using @racket[contract-out].

The @racket[arg-spec] specifies the names of arguments and the 
default values, which are not
normally written as part of a contract. They are combined with the
contract expression to generate the description of the binding in the
documentation via @racket[defproc]. The @racket[(arg-id default-expr)]
pairs specify the names and default values of the optional arguments.
If the contract supports optional arguments, then the first
@racket[arg-spec]s form must be used, otherwise the second must be used.

The @racket[desc-expr] is a sequence of documentation-time expressions that
produces prose to describe the exported binding---that is, the last
part of the generated @racket[defproc], so the description can refer
to the @racket[arg-id]s using @racket[racket].

The normal @racket[require]s of the enclosing library are effectively
converted into @racket[for-label] @racket[require]s when generating
documentation, so that identifiers in the @racket[contract]s are
linked to their corresponding documentation. Similarly, any binding
that is available in the run-time phase of the enclosing library
can be referenced in documentation prose using the @racket[racket]
form.}

@defform/subs[#:literals (-> ->i ->d values)
              (proc-doc id contract maybe-defs (desc-expr ...))
              ([contract (-> result)
                         (->i (arg ...) (opt ...) maybe-pre [id res])
                         (->i (arg ...) (opt ...) maybe-pre (values [id res] ...))
                         (->i (arg ...) (opt ...) #:rest rest [id result-expr])

                         (->d (arg ...) () maybe-pre (values [id result] ...))
                         (->d (arg ...) () maybe-pre [id result])
                         (->d (arg ...) () #:rest id rest [id result])]
               [maybe-pre (code:line)
                          (code:line #:pre (pre-id ...) condition)]
               [maybe-defs (code:line)
                           (default-expr default-expr ...)])]{

Like @racket[proc-doc/names], but supporting contract forms that embed
argument identifiers. Only a subset of @racket[->i] and @racket[->d] forms are
currently supported.

If the sequence of optional arguments, @racket[(opt ...)] is empty then
the @racket[maybe-arg-desc] must be not be present. If it is non-empty,
then it must have as many default expressions are there are optional
arguments.
}


@defform[(thing-doc id contract-expr (desc-expr ...))]{

Like @racket[proc-doc], but for an export of an arbitrary value.}


@defform[#:literals (parameter/c)
         (parameter-doc id (parameter/c contract-expr) arg-id (desc-expr ...))]{

Like @racket[proc-doc], but for exporting a parameter.}

@defform[(struct*-doc struct-name 
                      ([field-name contract-expr-datum] ...)
                      maybe-omit-constructor
                      maybe-mutable maybe-non-opaque maybe-constructor
                      (desc-expr ...))
         #:grammar ([maybe-omit-constructor (code:line) #:omit-constructor])]{
  Like @racket[proc-doc], but for struct declarations that use @racket[struct].
  
  The @racket[maybe-mutable], @racket[maybe-non-opaque], and @racket[maybe-constructor]
  options are as in @racket[defstruct].
}

@defform[(struct-doc struct-name 
                     ([field-name contract-expr-datum] ...) 
                     maybe-omit-constructor
                     maybe-mutable maybe-non-opaque maybe-constructor
                     (desc-expr ...))]{
  Like @racket[struct*-doc], but for struct declarations that use @racket[define-struct].
}


@defform/subs[(form-doc options form-datum
                maybe-grammar maybe-contracts
                (desc-expr ...))
              ([options (code:line maybe-kind maybe-link maybe-id maybe-literals)]
               [maybe-kind code:blank
                           (code:line #:kind kind-string-expr)]
               [maybe-link code:blank
                           (code:line #:link-target? link-target?-expr)]
               [maybe-id code:blank
                         (code:line #:id id)
                         (code:line #:id [id id-expr])]
               [maybe-literals code:blank
                               (code:line #:literals (literal-id ...))]
               [maybe-grammar code:blank
                              (code:line #:grammar ([nonterm-id clause-datum ...+] ...))]
               [maybe-contracts code:blank
                                (code:line #:contracts ([subform-datum contract-expr-datum]
                                                        ...))])]{

Like @racket[proc-doc], but for an export of a syntactic form. If
@racket[#:id] is provided, then @racket[id] is the exported identifier,
otherwise the exported identifier is extracted from @racket[form-datum].

See @racket[defform] for information on @racket[options],
@racket[form-datum], @racket[maybe-grammar], and
@racket[maybe-contracts].

@history[#:added "1.6"]}


@defform[(begin-for-doc form ...)]{

Like to @racket[begin-for-syntax], but for documentation time instead
of expansion time. The @racket[form]s can refer to binding
@racket[require]d with @racket[for-doc].

For example, a definition in @racket[begin-for-doc]
can be referenced by a @racket[_desc-expr] in
@racket[proc-doc/names].}


@defform[(generate-delayed-documents)]{

Causes documentation information to be recorded as a macro that is
expanded (along with any @racket[for-doc] imports) in the
module that uses @racket[include-extracted] or @racket[provide-extracted],
instead of within (a submodule of) the module that declares the information.

Delaying document generation in this way allows @racket[(for-doc
(for-label ....))]  imports that would otherwise create cyclic module
dependencies.

To avoid problems with accumulated @racket[for-doc] imports across
modules, @racket[generate-delayed-documents] declaration should appear
before any @racket[for-doc] import.}


@defform[(require/doc require-spec ...)]{

A legacy shorthand for @racket[(require (for-doc require-spec ...))].}


@defform[(provide/doc spec ...)]{

A legacy alternative to @racket[(provide spec ...)]}

@; ----------------------------------------

@section{Extracting Documentation from Source}

@defmodule[scribble/extract]

@defform[(include-extracted module-path)]{

Expands to a sequence of documentation forms extracted from
@racket[module-path], which is expected to be a module that uses
@racketmodname[scribble/srcdoc] (so that the module has a
@racketidfont{srcdoc} submodule).}

@defform[(provide-extracted module-path)]{

Similar to @racket[include-extracted], but the documentation is
packaged and exported as @racket[exported], instead of left
inline.

Use this form in combination with
@racket[include-previously-extracted] when documentation from a single
source is to be split and typeset among multiple documentation
locations. The @racket[provide-extracted] form extracts the
documentation once, and then @racket[include-previously-extracted]
form extracts documentation for specific bindings as needed.}

@defform[(include-previously-extracted module-path regexp)]{

Similar to @racket[include-extracted], but instead of referring to the
source that contains its own documentation, @racket[module-path]
refers to a module that uses @racket[provide-extracted]. The
@racket[include-previously-extracted] form expands to documentation
forms for all identifiers whose string forms match @racket[regexp].}
