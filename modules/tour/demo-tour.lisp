(in-package :story)

(define-demo tour ((:tour))
  (:h1 "cursors")
  (:div :id "cursors"
        (iter (for cursor in '(default help pointer progress crosshair
                               text vertical-text alias copy move no-drop
                               col-resize row-resize n-resize
                               e-resize s-resize w-resize ne-resize nw-resize
                               se-resize ew-resize ns-resize nesw-resize
                               nwse-resize zoom-in zoom-out))
          (htm (:span :class "cursor pack" :style (format nil "cursor:~(~A~);" cursor)
                      (fmt "~(~A~)" cursor)))))
  (script (pack "cursors" :gutter 2)))

(in-package :story-css)

(defun tour-css ()
  (css
   '((".cursor" :display inline-block :border "1px solid #999" :width 70 :height 40
      :padding 10px
      ))))

