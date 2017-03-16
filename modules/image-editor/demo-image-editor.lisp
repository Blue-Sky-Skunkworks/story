(in-package :story)


(define-demo image-editor ((:image-editor)
                           :directories (("modules/demo-images" "samples")
                                         ("/home/student/p/guests/signs/" "signs"))
                           :imports (("ied" image-editor-style))
                           )
  (:div :id "controls")
  (:canvas :id "canvas" :width 800 :height 600)
  (:span :id "modeline")
  (script
    (initialize-image-editor "canvas" "controls" "modeline")
    (load-image "signs/s16.png")))


(in-package :story-css)

(defun image-editor-style ()
  (style ()
    `((body :font "roboto" :margin 0 :height 100vh :color white :background-color black)
      (paper-button :color white)
      ("#canvas" :background-color "#111"))))



