(in-package :story)

(define-story-module polymer
    :directories (("imports" "polymer") ("webcomponentsjs" "js/webcomponentsjs"))
    :scripts ("/webcomponentsjs/webcomponents-lite.js")
    :imports ("polymer/polymer"))

(define-demo polymer (:polymer)
  )

(define-story-module paper-material
    :extends :polymer
    :imports ("paper-material/paper-material"))

(defmacro material (&body body)
  `(html (:paper-material ,@body)))

(define-demo paper-material (:paper-material)
  (material :style "width:300px;height:300px;margin:20px;"))

