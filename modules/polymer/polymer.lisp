(in-package :story)

(define-story-module polymer
    :directories (("imports" "polymer") ("webcomponentsjs" "js/webcomponentsjs"))
    :scripts ("/webcomponentsjs/webcomponents-lite.js")
    :imports ("polymer/polymer"))

(define-story-module paper-material
    :extends :polymer
    :imports ("paper-material/paper-material"))

(macrolet
    ((define-polymer-macros (prefix &rest names)
       `(progn
          ,@(iter (for name in names)
                  (collect
                      `(defmacro ,name (&body body)
                         `(html (,,(ksymb prefix '- name ) ,@body))))))))
  (define-polymer-macros paper material)
  (define-polymer-macros iron meta))

(define-story-module paper-material
    :extends :polymer
    :imports ("paper-material/paper-material"))

(define-story-module iron-meta
    :extends :polymer
    :imports ("iron-meta/iron-meta"))

(define-story-module iron-flex-layout
    :extends :polymer
    :imports ("iron-flex-layout/iron-flex-layout-classes"))
