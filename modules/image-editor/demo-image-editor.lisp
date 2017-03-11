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
(defpsmacro with-image ((var url) &body body)
  `((@ fabric *image from-u-r-l) ,url (lambda (,var) ,@body)))

(define-script image-editor

  (defun nround (x n)
    (if n
        (let ((n ((@ *math pow) 10 n)))
          (/ ((@ *math round) (* x n)) n))
        ((@ *math round) x)))

  (defvar *canvas*)
  (defvar *container*)
  (defvar *mouse-down*)

  (defvar *modeline*)
  (defun create-modeline ()
    (setf *modeline* (text "" :stroke "white" :fill "white"
                              :left 0 :top (- (@ *container* height) 50))))

  (defvar *crop*)

  (defun update-modeline ()
    ((@ *modeline* set-text) (+ "(" (nround (left *crop*))
                                "+" (width *crop*)
                                "," (nround (top *crop*))
                                "+" (height *crop*) ")")))

  (defun create-cropper ()
    (setf *crop* (rect fill "transparent"
                       stroke "white"
                       stroke-dash-array (array 2 -2)
                       visible false
                       border-color "white"
                       corner-color "white"
                       corner-size 20
                       has-rotating-point false))
    (on *crop* "moving" (update-modeline))
    (on *crop* "scaling" (update-modeline)))

  (defun initialize-image-editor (canvas-id)
    (setf *canvas* (new ((@ fabric *canvas) canvas-id))
          *container* ((@ (id canvas-id) get-bounding-client-rect)))
    (create-modeline)
    (create-cropper)
    (add *crop* *modeline*)
    (on *canvas* "mouse:down"
        (let ((left (- (@ event e page-x) (left *container*)))
              (top (- (@ event e page-y) (top *container*))))
          (when (not (@ *crop* visible))
            (setf (width *crop*) 2
                    (height *crop*) 2
                    (left *crop*) left
                    (top *crop*) top
                    (@ *crop* visible) t)
            (setf *mouse-down* (@ event e))
            (bring-to-front *crop*))))
    (on *canvas* "mouse:up" (setf *mouse-down* nil))
    (on *canvas* "mouse:move"
        (when *mouse-down*
          (setf (width *crop*) (- (@ event e page-x) (@ *mouse-down* page-x))
                (height *crop*) (- (@ event e page-y) (@ *mouse-down* page-y)))
          ((@ *crop* set-coords))
          (update-modeline)
          ((@ *canvas* render-all)))))
  (defvar *image*)
  (defun load-image (url)
    (with-image (img url)
      (setf *image* img)
      ((@ img set) (create selectable false))
      (add img))))
