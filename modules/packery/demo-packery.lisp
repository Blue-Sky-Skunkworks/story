(in-package :story)

(define-demo packery (:packery)
  (:div :id "pack"
        (iter (for x from 1 to 10)
              (htm (:div :class "el" :style "width:200;height:200;background:blue;"))))
  (script (pack "pack" "el")))
