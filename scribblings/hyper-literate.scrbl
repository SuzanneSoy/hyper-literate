#lang scribble/manual
@require[@for-label[;hyper-literate
 racket/base]]

@title{hyper-literate}
@author{Georges Dupéron}

@(require scribble/manual
          scribble/core
          scribble/decode
          scribble/racket
          (only-in scribble/racket value-link-color))

@defmodulelang[hyper-literate]

The @racketmodname[hyper-literate] metalanguage extends the
features of @racketmodname[scribble/lp2], with the goal of
providing a more modern view on literate programming. It can
be parameterized with the language used in the chunks (so
that it is possible to directly write 
@racketmodname[typed/racket] programs with 
@racketmodname[hyper-literate], for example).

On the first line, which begins with @tt{@litchar{#lang}
 @racketmodname[hyper-literate]}, the language recognises the following
options:

@(require scribble/core
          (only-in scribble/private/manual-vars boxed-style)
          scribble/private/manual-utils)
@(make-table
  boxed-style
  (list
   (list
    @paragraph[(style #f '())]{
 @tt{@litchar{#lang} @racketmodname[hyper-literate] @racket[_lang]
  @racket[_maybe-no-req] @racket[_maybe-no-auto]}})
   flow-empty-line
   (list
    @racketgrammar*[
 [maybe-no-req #:no-require-lang]
 [maybe-no-auto #:no-auto-require]])))

where @racket[_lang] is a module name which can be used as
a @litchar{#lang}, for example @racketmodname[typed/racket]
or @racketmodname[racket/base].

The current implementation of hyper-literate needs to inject
a @racket[(require _lang)] in the expanded module, in order
to have the arrows properly working in DrRacket for
"built-in" identifiers which are provided by the 
@racket[_lang] itself. This extra @racket[require] statement
can however conflict with later user-provided 
@racket[require] statements, which would otherwise shadow
the built-ins. The @racket[#:no-require-lang] option
disables that behaviour, and has the only drawback that
built-ins of the @racket[_lang] language do not have an
arrow in DrRacket (but they still should be highlighted with
a turquoise background when hovered with the mouse).

The current implementation of @racketmodname[scribble/lp2],
on which @racketmodname[hyper-literate] relies (with a few
changes), extracts the @racket[require] statements from
chunks of code, and passes them to 
@racket[(require (for-label …))]. The goal is to have
identifiers from required modules automatically highlighted
and hyperlinked to their documentation. However, all
meta-levels are smashed into the @racket[#f] i.e. 
@racket[for-syntax] meta-level. As a consequence, conflicts
can arise at the @racket[for-label] meta-level between two
modules, even if these two modules were originally required
at distinct meta-levels in the source program. It is
possible in this case to disable the feature using 
@racket[#:no-auto-require], and to manually call 
@racket[(require (for-label …))] and handle conflicting
identifiers in a more fine-grained way.

@section{What is hyper-literate programming?}

Hyper-literate programming is to literate programming
exactly what hypertext documents are to regular books and
texts. Literate programming is about telling other
programmers how the program works (instead of just telling
the compiler what it does). Telling this story can be done
using non-linear, hyperlinked documents.

For now these utilities only help with manipulating literate
programming chunks (e.g. repeating the same chunk in several
places in the output document, but keep a single copy in the
source code).

Ultimately, the reading experience should be closer to
viewing an interactive presentation, focusing on the parts
of the program that are of interest to you: expand on-screen
the chunks you are curious about, run some tests and see
their result, etc.

@itemlist[
 @item{Imagine something like 
  @hyperlink["http://www.andrewbragdon.com/codebubbles_site.asp"]{
   code bubbles}, but with explanatory text coming along
  with the source code.}
 @item{Imagine something like 
  @hyperlink["http://inform7.com/"]{Inform}, but focused on
  exploring a program instead of exploring an imaginary
  world — after all, a program is some kind of imaginary
  world.}]

@section{Chunks of code}

@; @racket[chunk] does not work for these, probably due to the use of either:
@;   @title[#:tag "lp" …]{Literate Programming}
@; or:
@;   @defmodulelang[scribble/lp2 #:use-sources (scribble/lp)]{…}
@; in scribble-doc/scribblings/scribble/lp.scrbl
@; See scribble bug #51 https://github.com/racket/scribble/issues/51
@(define scribble-chunk
   (element symbol-color
            (make-link-element value-link-color
                               (decode-content (list "chunk"))
                               '(form ((lib "scribble/lp.rkt") chunk)))))
@(define scribble-CHUNK
   (element symbol-color
            (make-link-element value-link-color
                               (decode-content (list "CHUNK"))
                               '(form ((lib "scribble/lp.rkt") CHUNK)))))

@;{
 @(module scribble-doc-links racket/base
    (require scribble/manual
             (for-label scribble/lp2
                        scribble/private/lp))
    (provide (all-defined-out))
    (define scribble-chunk @racket[chunk])
    (define scribble-CHUNK @racket[CHUNK]))
 @(require 'scribble-doc-links)
}

@defform[(chunk <name> content ...)]{
 Same as @scribble-chunk from @racketmodname[scribble/lp2],
 with a few tweaks and bug fixes.}

@defform[(CHUNK <name> content ...)]{
 Same as @scribble-CHUNK from @racketmodname[scribble/lp2],
 with a few tweaks and bug fixes.}

@section{Memorizing and repeating chunks}

@defform[(defck <name> content ...)]{
 Like @racket[chunk] from @racketmodname[scribble/lp2], but
 remembers the chunk so that it can be re-displayed later
 using @racket[repeat-chunk].}

@defform[(repeat-chunk <name>)]{
 Shows again a @racket[chunk] of code previously remembered
 with @racket[defck]. If the @racket[<name>] starts and
 ends with angle brackets, they are replaced by parentheses
 to hint that this is not the first occurrence of this
 chunk, so that the name becomes @racket[|(name)|]}

@section{Order of expansion of the program}

The file is expanded a first time, in order to identify and
aggregate the chunks of code (declared with @racket[chunk]).
Then, the top-level module of the file is constructed using
these chunks, and a @racket[doc] submodule is added, which
contains all the surrounding text. The chunks are typeset
where they appear using @racket[racketblock].

The @racket[doc] submodule is declared using 
@racket[module*], so that it can use 
@racket[(require (submod ".."))] to use functions declared
in the the chunks. For example, it should be possible to
dynamically compute the result of a function, and insert it
into the document, so that the value displayed always
matches the implementation.

When the file is expanded for the first time, however, the 
@racket[(submod "..")] does not exist yet, and cannot be
required. This is the case because the first expansion is
performed precisely to extract the chunks and inject them in
that module.

To solve this problem, the following macros behave
differently depending on whether the code is being expanded
for the first time or not (in which case the 
@racket[(submod "..")] module can be used).

@defform[(if-preexpanding a b)]{
 Expands to @racket[a] if the code is being pre-expanded,
 and expands to @racket[b] if the @racket[(submod "..")]
 module can be used.}

@defform[(when-preexpanding . body)]{
 Expands to @racket[(begin . body)] if the code is being
 pre-expanded, and expands to @racket[(begin)] otherwise.}

@defform[(unless-preexpanding . body)]{
 Expands to @racket[(begin . body)] if the @racket[(submod "..")]
 module can be used, and expands to @racket[(begin)] otherwise.}
