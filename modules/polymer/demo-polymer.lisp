(in-package :story)

(define-demo polymer (:polymer)
  (script (*polymer (create :is "proto-element"
                            :ready (lambda () (setf (@ this text-content) "Polymer works!")))))
  (:proto-element))

(define-demo paper-material (:paper-material)
  (material :style "width:300px;height:300px;margin:20px;"))

(define-demo iron-meta (:iron-meta)
  (meta :id "id" :key "foo" :value "Iron Meta has worked.")
  (:div :id "result" "Original Value")
  (script (set-html "result" ((@ (id "id") by-key) "foo"))))

(define-demo iron-flex-layout (:iron-flex-layout)
  (:style :is "custom-style" :include "iron-flex iron-flex-alignment")
  (:div :class "layout horizontal"
        (:div :style "width:100px;height:100px;background:blue;")
        (:div :class "flex" :style "width:100px;height:100px;background:green;")
        (:div :style "width:100px;height:100px;background:blue;")))

(define-demo iron-icon (:iron-icons :iron-icon)
  (icon :icon "thumb-up"))

(define-demo iron-pages (:iron-pages)
  (pages :selected 1
    (:div "Iron Pages does not work!!")
    (:div "Iron Pages works!")))
