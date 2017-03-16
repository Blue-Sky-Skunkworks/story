(in-package :story)

(define-demo image-editor ((:image-editor)
                           :directories (("modules/demo-images" "samples")
                                         ("/home/student/p/guests/signs/" "signs"))
                           :scripts (("ied.js" image-editor))
                           :imports (("ied" image-editor-style)))
  (:canvas :id "canvas" :width 800 :height 600)
  (:span :id "modeline")
  (script
    (initialize-image-editor "canvas" "modeline")
    (load-image "signs/s16.png")))

(in-package :story-css)

(defun image-editor-style ()
  (style ()
    `((body :font "roboto" :margin 0 :height 100vh :color white :background-color black)
      ("#canvas" :background-color "#111"))))

(in-package :story-js)

(define-script image-editor

  (defvar *editor*)

  (defun nround (x n)
    (if n
        (let ((n ((@ *math pow) 10 n)))
          (/ ((@ *math round) (* x n)) n))
        ((@ *math round) x)))

  (defvar *mouse-down*)

  (defun update-modeline ()
    (when (@ *editor* modeline)
      (let ((crop (@ *editor* crop)))
       (set-html (@ *editor* modeline)
                 (+ "(" (nround (left crop))
                    "+" (width crop)
                    "," (nround (top crop))
                    "+" (height crop) ")")))))

  (defun create-cropper ()
    (let ((crop (rect fill "transparent"
                      stroke "red"
                      stroke-dash-array (array 2 -2)
                      visible false
                      border-color "white"
                      corner-color "white"
                      corner-size 20
                      has-rotating-point false)))
      (setf (@ *editor* crop) crop)
      (on crop "moving" (update-modeline))
      (on crop "scaling" (update-modeline))
      crop))

  (defun initialize-image-editor (canvas-id &optional modeline-id)
    (setf *editor* (initialize-fabric canvas-id))
    (when modeline-id (setf (@ *editor* modeline) (id modeline-id)))
    (with-fabric *editor*
      (add (create-cropper)))
    (on *editor* "mouse:down"
        (with-fabric *editor*
          (let ((left (- (@ event e page-x) (left (@ *editor* bounds))))
                (top (- (@ event e page-y) (top (@ *editor* bounds))))
                (crop (@ *editor* crop)))
            (when (not (@ crop visible))
              (setf (width crop) 2
                    (height crop) 2
                    (left crop) left
                    (top crop) top
                    (@ crop visible) t)
              (setf *mouse-down* (@ event e))
              (bring-to-front crop)))))
    (on *editor* "mouse:up" (setf *mouse-down* nil))
    (on *editor* "mouse:move"
        (when *mouse-down*
          (let ((crop (@ *editor* crop)))
            (setf (width crop) (- (@ event e page-x) (@ *mouse-down* page-x))
                  (height crop) (- (@ event e page-y) (@ *mouse-down* page-y)))
            ((@ crop set-coords)))
          (update-modeline)
          ((@ *editor* render-all)))))

  (defun load-image (url)
    (with-image (img url)
      (with-fabric *editor*
        ((@ img set) (create selectable false))
        (add img)))))
