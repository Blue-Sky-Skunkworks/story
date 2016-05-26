(in-package :story)

(define-demo packery ((:packery))
  (:div :id "id"
        (iter (for x from 1 to 10)
          (htm (:div :class "pack" :style "width:200;height:200;background:blue;"))))
  (script (pack "id")))


