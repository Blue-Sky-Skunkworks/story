(in-package :story)

(defmacro  define-polymer-macros (prefix &rest names)
  `(progn
     ,@(iter (for name in names)
             (appending
              `((defmacro ,name (&body body)
                  `(html (,,(ksymb prefix '- name ) ,@body)))
                (export ',name))))))

(define-story-module polymer
    :directories (("imports" "polymer") "webcomponentsjs")
    :scripts ("/webcomponentsjs/webcomponents-lite.js")
    :imports ("polymer/polymer"))

(defmacro define-polymer-module (name &key (helpers t))
  (let*  ((raw (symbol-name name))
          (pos (position #\- raw))
          (module (subseq raw 0 pos))
          (rest (subseq raw (1+ pos))))
    `(progn
       (define-story-module ,name
           :extends :polymer
           :imports (,(format nil "~(~A/~A~)" name name)))
       ,@(when helpers `((define-polymer-macros ,(symb module) ,(symb rest)))))))

(define-polymer-module iron-meta)
(define-story-module iron-flex-layout :extends :polymer :imports ("iron-flex-layout/iron-flex-layout-classes"))
(define-polymer-module iron-icons)
(define-polymer-module iron-icon)
(define-polymer-module iron-pages)
(define-polymer-module paper-button)
(define-polymer-module paper-icon-button)
(define-polymer-module paper-material)
(define-polymer-module paper-card)
(define-polymer-module paper-ripple)
(define-polymer-module paper-fab)
(define-polymer-module paper-item)
(define-polymer-module paper-header-panel)
(define-polymer-module paper-toolbar)

(define-story-module neon-animated-pages :extends :polymer :imports ("neon-animation/neon-animated-pages"))

(define-story-module neon-animatable :extends :polymer :imports ("neon-animation/neon-animatable")
                     :production-import-fix neon-animation-import-fix)

(defun neon-animation-import-fix (text stream)
  (multiple-value-bind (ms me) (scan (create-scanner "\\.\\./web-animations-js" :single-line-mode t) text)
    (princ (subseq text 0 ms) stream)
    (princ "polymer/web-animations-js" stream)
    (princ (subseq text me) stream)))

(define-polymer-macros neon animatable animated-pages)

(defmacro define-neon-animations (&rest names)
  `(progn
     ,@ (iter (for name in names)
              (collect `(define-story-module ,(symb name '-animation)
                            :extends :polymer
                            :imports (,(format nil "neon-animation/animations/~(~A~)-animation" name)))))))

(define-neon-animations fade-in fade-out scale-down scale-up slide-down slide-up
                        slide-from-top slide-from-bottom slide-left slide-right
                        slide-from-left slide-from-right transform hero
                        ripple reverse-ripple cascase opaque)

(define-story-module google-map :extends :polymer :imports ("google-map/google-map"))
(defmacro google-map (&body body) `(html (:google-map ,@body)))
(defmacro map-marker (&body body) `(html (:google-map-marker ,@body)))
(export '(google-map map-marker))
