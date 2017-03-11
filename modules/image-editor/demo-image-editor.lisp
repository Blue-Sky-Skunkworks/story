(in-package :story)

(define-demo image-editor ((:image-editor :iron-icons :iron-icons-image :iron-icon
                                          :paper-icon-button)
                           :directories (("modules/demo-images" "samples"))
                           :scripts (("ied.js" image-editor))
                           :imports (("ied" image-editor-style)))
  (icon-button :icon "zoom-in")
  (icon-button :icon "zoom-out")
  (:canvas :id "canvas" :width 800 :height 600)
  (script
    (initialize-image-editor "canvas")
    (load-image "samples/peltier-painting.png")))

(in-package :story-css)

(defun image-editor-style ()
  (style ()
    `((body :font "roboto" :margin 0 :height 100vh :color white :background-color black)
      ("#canvas" :background-color "#111"))))

(in-package :story-js)

(defpsmacro rect (&rest args) `(new ((@ fabric *rect) (create ,@args))))
(defpsmacro on (el event-name &body body)
  `((@ ,el on) ,event-name (lambda (event) ,@body)))

(define-script image-editor
  (defvar *canvas*)
  (defvar *container*)
  (defvar *cropper*)
  (defvar *mouse-down*)
  (defvar *moving*)
  (defun initialize-image-editor (canvas-id)
    (setf *canvas* (new ((@ fabric *canvas) canvas-id))
          *container* ((@ (id canvas-id) get-bounding-client-rect))
          *cropper* (rect fill "transparent"
                          stroke "#ccc"
                          stroke-dash-array (array 2 -2)
                          visible false))
    ((@ *canvas* add) *cropper*)
    (defun point-inside (el x y)
      (and (>= x (@ el left)) (<= x (+ (@ el left) (@ el width)))
           (>= y (@ el top)) (<= y (+ (@ el top) (@ el height)))))
    (on *canvas* "mouse:down"
        (let ((left (- (@ event e page-x) (@ *container* left)))
              (top (- (@ event e page-y) (@ *container* top))))
          (if (point-inside *cropper* left top)
              (setf *moving* (create :x (@ *cropper* left) :y (@ *cropper* top)))
              (setf (@ *cropper* width) 2
                    (@ *cropper* height) 2
                    (@ *cropper* left) left
                    (@ *cropper* top) top
                    (@ *cropper* visible) t
                    *moving* nil))
          (setf *mouse-down* (@ event e)))
        ((@  *canvas* bring-to-front) *cropper*))
    (on *canvas* "mouse:up"
        (setf *mouse-down* nil))
    (on *canvas* "mouse:move"
        (when *mouse-down*
          (if *moving*
              (setf (@ *cropper* top) (+ (@ *moving* y) (- (@ event e page-y) (@ *mouse-down* page-y)))
                    (@ *cropper* left) (+ (@ *moving* x) (- (@ event e page-x) (@ *mouse-down* page-x))))
              (setf (@ *cropper* width) (- (@ event e page-x) (@ *mouse-down* page-x))
                    (@ *cropper* height) (- (@ event e page-y) (@ *mouse-down* page-y))))
          ((@ *canvas* render-all)))))
  (defvar *image*)
  (defun load-image (url)
    ((@ fabric *image from-u-r-l) url
     (lambda (img) (setf *image* img)
       ((@ img set) (create selectable false))
       ((@ *canvas* add) img)))))
