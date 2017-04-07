(in-package :story)

(define-story-module colors
  :scripts ("tinycolor.js")
  :imports (("color-demo" color-demo-template))
  :depends-on (:polymer :paper-input))

(define-template color-demo
  :properties (("color" string "red"))
  :style (("#top" :max-width 200 :margin 20px)
          ("#demo" :width 100 :height 100 :margin-top 20px)
          ("#selection" :max-width 100))
  :content ((:div :id "top"
                  (input :id "color" :value "{{color}}" :on-keypress "colorKeys")
                  (:div :id "demo")))
  :methods
  ((_set ()
         (with-content (demo color)
           (setf (@ demo style background) (@ color value))
           ((@ color focus))))
   (attached () (_set))
   (color-keys (event) (when (eql (@ event key) "Enter") (_set)))))
