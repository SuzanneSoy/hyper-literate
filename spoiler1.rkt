#lang racket

(provide spoiler-wrapper-collapsed
         spoiler-default
         spoiler-alt
         spoiler-button-default-to-alt
         spoiler-button-alt-to-default
         spoiler1
         spler)

(require scribble/manual
         scribble/core
         scribble/decode
         scribble/html-properties
         hyper-literate
         (for-syntax syntax/parse)
         scriblib/render-cond)

(define spoiler-css
  #"
.spoiler-wrapper-expanded .spoiler-default,
.spoiler-wrapper-expanded .spoiler-button-default-to-alt {
  display:none;
}
.spoiler-wrapper-collapsed .spoiler-alt,
.spoiler-wrapper-collapsed .spoiler-button-alt-to-default {
  display:none;
}

.spoiler-button-default-to-alt,
.spoiler-button-alt-to-default {
  color: #2a657e;
}
")

(define spoiler-js
  (string->bytes/utf-8
   #<<EOJS
function toggleSpoiler(e, doExpand) {
 var expanded = function(className) {
   return className.match(/\bspoiler-wrapper-expanded\b/);
  };
  var collapsed = function(className) {
    return className.match(/\bspoiler-wrapper-collapsed\b/);
  };
  var found = function(className) {
    return expanded(className) || collapsed(className);
  };
  var wrapper = e;
  while (e != document && e != null && ! found(e.className)) {
    e = e.parentNode;
  }
  e.className = e
                .className
                .replace(/  */g, " ")
                .replace(/\bspoiler-wrapper-expanded\b/, '')
                .replace(/\bspoiler-wrapper-collapsed\b/, '');
  if (doExpand) {
    e.className = e.className + " spoiler-wrapper-expanded";
  } else {
    e.className = e.className + " spoiler-wrapper-collapsed";
  }
  if (e.preventDefault) { e.preventDefault(); }
  return false;
}
EOJS
   ))

(define-syntax-rule (def-style name)
  (define name
    (style (symbol->string 'name)
           (list (css-addition spoiler-css)
                 (js-addition spoiler-js)
                 (alt-tag "div")))))

(def-style spoiler-wrapper-collapsed)
(def-style spoiler-default)
(def-style spoiler-alt)

(define (spoiler-button-default-to-alt txt)
  (hyperlink
   #:style (style "spoiler-button-default-to-alt"
                  (list (css-addition spoiler-css)
                        (js-addition spoiler-js)
                        (attributes
                         '([onclick . "return toggleSpoiler(this, true);"]))))
   "#"
   txt))

(define (spoiler-button-alt-to-default txt)
  (hyperlink
   #:style (style "spoiler-button-alt-to-default"
                  (list (css-addition spoiler-css)
                        (js-addition spoiler-js)
                        (attributes
                         '([onclick . "return toggleSpoiler(this, false);"]))))
   "#"
   txt))

(define (spoiler1 default button-default→alt button-alt→default alternate)
  (nested-flow spoiler-wrapper-collapsed
               (list
                (paragraph (style #f '())
                           (spoiler-button-default-to-alt button-default→alt))
                (nested-flow spoiler-default
                             (decode-flow default))
                (paragraph (style #f '())
                           (spoiler-button-alt-to-default button-alt→default))
                (nested-flow spoiler-alt
                             (decode-flow alternate)))))

(define-syntax spler
  (syntax-parser
    [(_ name default ... #:expanded expanded ...)
     #'(begin
         (chunk #:save-as ck1
                #:display-only
                #:button
                (cond-element
                 [html (list " " (smaller
                                  (spoiler-button-default-to-alt "expand")))]
                 [else (list)])
                name
                default ...)

         (chunk #:save-as ck2
                #:button
                (cond-element
                 [html (list " " (smaller
                                  (spoiler-button-alt-to-default "collapse")))]
                 [else (list)])
                name
                expanded ...)

         (cond-block
          [html (nested-flow spoiler-wrapper-collapsed
                             (list (nested-flow spoiler-default
                                                (decode-flow (ck1)))
                                   (nested-flow spoiler-alt
                                                (decode-flow (ck2)))))]
          [else (nested-flow (style #f '())
                             (decode-flow (ck2)))]))]))