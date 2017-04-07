(in-package :story)

(define-demo polymer ((:polymer))
  (script (*polymer (create :is "proto-element"
                            :ready (lambda () (setf (@ this text-content) "Polymer works!")))))
  (:proto-element))

(define-demo iron-meta ((:iron-meta))
  (meta :id "id" :key "foo" :value "Iron Meta has worked.")
  (:div :id "result" "Original Value")
  (script (set-html "result" ((@ (id "id") by-key) "foo"))))

(define-demo iron-flex-layout ((:iron-flex-layout))
  (:style :is "custom-style" :include "iron-flex iron-flex-alignment")
  (:div :class "layout horizontal"
        (:div :style "width:100px;height:100px;background:blue;")
        (:div :class "flex" :style "width:100px;height:100px;background:green;")
        (:div :style "width:100px;height:100px;background:blue;")))

(define-demo iron-icon ((:iron-icons :iron-icon))
  (icon :icon "thumb-up"))

(define-demo iron-pages ((:iron-pages))
  (pages :selected 1
    (:div "Iron Pages does not work!!")
    (:div "Iron Pages works!")))

(define-demo iron-request ((:iron-request) :dispatches ((:prefix "/test-iron-request" handle-test-iron-request)))
  (:div :id "id" "Original Value")
  (script (request "/test-iron-request" (lambda (val) (set-html "id" (@ val response))))))

(defun handle-test-iron-request (&rest args) "Iron Request works!")

(define-demo iron-collapse ((:iron-collapse))
  (:button :onclick (ps (toggle)) "toggle")
  (collapse :id "id" (:div "Iron Collapse Works"))
  (script (defun toggle () ((@ (id "id") toggle)))))

(define-demo iron-image ((:iron-image) :directories (("modules/demo-images" "demo-images")))
  (image :src "demo-images/peltier-painting.jpg")
  (:br)
  (image :src "demo-images/peltier-painting.jpg" :style "width:200px;height:100px;" :sizing "cover"))

(define-demo paper-button ((:paper-button))
  (button :style "background:blue;color:white;" "Paper Buttons work!"))

(define-demo paper-icon-button ((:paper-icon-button :iron-icons))
  (icon-button :icon "thumb-up"))

(define-demo paper-material ((:paper-material))
  (material :style "width:300px;height:300px;margin:20px;"))

(define-demo paper-card ((:paper-card))
  (card :heading "Paper Cards works!"
        (:div :class "card-content" "content")
        (:div :class "card-actions" "action")))

(define-demo paper-ripple ((:paper-ripple))
  (:div :style "width:100%;height:100%;" (ripple)))

(define-demo paper-fab ((:paper-fab :iron-icons))
  (fab :icon "thumb-up"))

(define-demo paper-item ((:paper-item))
  (item "Paper Items work!"))

(define-demo paper-header-panel ((:paper-header-panel))
  (header-panel
    (:div :class "paper-header" "Paper Headers work!")
    (:div "content")))

(define-demo paper-toolbar ((:paper-toolbar))
  (toolbar (:div "Paper Toolbars work!")))

(define-demo paper-listbox ((:paper-listbox :paper-item))
  (listbox
    (item "First Item")
    (item "Second Item")))

(define-demo paper-drawer-panel ((:paper-drawer-panel))
  (drawer-panel
    (:div :drawer t "Drawer")
    (:div :main t "Main")))

(define-demo neon-animated-pages ((:neon-animatable :neon-animated-pages :slide-from-right-animation :slide-left-animation))
  (animated-pages :selected 0 :id "id" :entry-animation "slide-from-right-animation" :exit-animation "slide-left-animation"
                  (with-words (word "Eeny meeny miny moe.")
                    (animatable (str word))))
  (:br) (:br)
  (:button :onclick (ps (toggle-page)) "next")
  (script*
    `(defun toggle-page ()
       (let ((el ((@ document get-element-by-id) "id")))
         (setf (@ el selected) (ps:% (+ (@ el selected) 1) 4))))))

(define-demo google-map ((:google-map))
  (:google-map :latitude "46.8787" :longitude "-113.9966"))

(define-demo templates ((:polymer)
                        :imports (("demo-element" demo-element-template)))
  (:demo-element)
  (:demo-element :title "WooHoo!"))

(define-template demo-element
  :style ((":host" :display inline-block :background-color "#AAA" :padding 20px :margin 10px))
  :properties (("title" string "nameme"))
  :content ((:span "This is {{title}}.")))
