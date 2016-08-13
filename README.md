[![Build Status,](https://img.shields.io/travis/jsmaniac/hyper-literate/master.svg)](https://travis-ci.org/jsmaniac/hyper-literate)
[![Coverage Status,](https://img.shields.io/coveralls/jsmaniac/hyper-literate/master.svg)](https://coveralls.io/github/jsmaniac/hyper-literate)
[![Build Stats,](https://img.shields.io/badge/build-stats-blue.svg)](http://jsmaniac.github.io/travis-stats/#jsmaniac/hyper-literate)
[![Online Documentation.](https://img.shields.io/badge/docs-online-blue.svg)](http://docs.racket-lang.org/hyper-literate/)

hyper-literate
==============

Some tools which help build hyper-literate programs.

Hyper-literate programming is to literate programming exactly what hypertext
documents are to regular books and texts. Literate programming is about
telling other programmers how the program works (instead of just telling the
compiler what it does). Telling this story can be done using non-linear,
hyperlinked documents.

For now these utilities only help with manipulating LP chunks (e.g. repeating
the same chunk in several places in the output document, but keep a single
copy in the source code).

Ultimately, the reading experience should be closer to viewing an interactive
presentation, focusing on the parts of the program that are of interest to
you: expand on-screen the chunks you are curious about, run some tests and see
their result, etc.

* Imagine something like [code
  bubbles](http://www.andrewbragdon.com/codebubbles_site.asp), but with
  explanatory text coming along with the source code.
  
* Imagine something like [Inform](http://inform7.com/), but focused on
  exploring a program instead of exploring an imaginary world — after all, a
  program is some kind of imaginary world.