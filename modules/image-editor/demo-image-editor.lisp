(in-package :story)

(define-demo image-editor ((:image-editor)
                           :directories (("modules/demo-images" "samples"))
                           :scripts (("ied.js" image-editor))
                           :imports (("ied" image-editor-style)))
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
(defpsmacro text (text &rest args) `(new ((@ fabric *text) ,text (create ,@args))))
(defpsmacro on (el event-name &body body)
  `((@ ,el on) ,event-name (lambda (event) ,@body)))
(defpsmacro add (&rest els) `(progn ,@(loop for el in els collect `((@ *canvas* add) ,el))))
(defpsmacro left (el) `(@ ,el left))
(defpsmacro top (el) `(@ ,el top))
(defpsmacro width (el) `(@ ,el width))
(defpsmacro height (el) `(@ ,el height))
(defpsmacro bring-to-front (el) `((@ *canvas* bring-to-front) ,el))

(define-script image-editor
  (defvar *canvas*)
  (defvar *container*)
  (defvar *crop*)
  (defvar *mouse-down*)
  (defvar *moving*)
  (defvar *modeline*)
  (defun initialize-image-editor (canvas-id)
    (setf *canvas* (new ((@ fabric *canvas) canvas-id))
          *container* ((@ (id canvas-id) get-bounding-client-rect))
          *modeline* (text "" :stroke "white" :fill "white"
                              :left 0 :top (- (@ *container* height) 50))
          *crop* (rect fill "transparent"
                       stroke "#ccc"
                       stroke-dash-array (array 2 -2)
                       visible false))
    (add *crop* *modeline*)
    (defun point-inside (el x y)
      (and (>= x (left el)) (<= x (+ (left el) (width el)))
           (>= y (top el)) (<= y (+ (top el) (height el)))))
    (on *canvas* "mouse:down"
        (let ((left (- (@ event e page-x) (left *container*)))
              (top (- (@ event e page-y) (top *container*))))
          (cond
            ((point-inside *crop* left top)
             (setf *moving* (create :x (left *crop*) :y (top *crop*))))
            (t (setf (width *crop*) 2
                     (height *crop*) 2
                     (left *crop*) left
                     (top *crop*) top
                     (@ *crop* visible) t
                     *moving* nil)))
          (setf *mouse-down* (@ event e)))
        (bring-to-front *crop*))
    (on *canvas* "mouse:up" (setf *mouse-down* nil))
    (on *canvas* "mouse:move"
        (when *mouse-down*
          (if *moving*
              (setf (top *crop*) (+ (@ *moving* y) (- (@ event e page-y) (@ *mouse-down* page-y)))
                    (left *crop*) (+ (@ *moving* x) (- (@ event e page-x) (@ *mouse-down* page-x))))
              (setf (width *crop*) (- (@ event e page-x) (@ *mouse-down* page-x))
                    (height *crop*) (- (@ event e page-y) (@ *mouse-down* page-y))))
          ((@ *modeline* set-text) (+ "(" (left *crop*) "+" (width *crop*) "," (top *crop*) "+" (height *crop*) ")"))
          ((@ *canvas* render-all)))))
  (defvar *image*)
  (defun load-image (url)
    ((@ fabric *image from-u-r-l) url
     (lambda (img) (setf *image* img)
       ((@ img set) (create selectable false))
       ((@ *canvas* add) img)))))
