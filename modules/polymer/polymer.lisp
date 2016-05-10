(in-package :story)


(defmacro  define-polymer-macros (prefix &rest names)
  `(progn
     ,@(iter (for name in names)
             (collect
                 `(defmacro ,name (&body body)
                    `(html (,,(ksymb prefix '- name ) ,@body)))))))

(define-story-module polymer
    :directories (("imports" "polymer") ("webcomponentsjs" "js/webcomponentsjs"))
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
