(in-package :story)

(define-story-module image-editor
  :scripts (("ied.js" image-editor-js))
  :depends-on (:fabric :files :polymer :iron-icons :iron-icons-image :paper-icon-button))

(in-package :story-js)

(define-script image-editor-js
  (defvar *editor*)

  (defun nround (x n)
    (if n
        (let ((n ((@ *math pow) 10 n)))
          (/ ((@ *math round) (* x n)) n))
        ((@ *math round) x)))

  (defun update-modeline ()
    (let* ((image (@ *editor* image))
           (zoom ((@ *editor* get-zoom)))
           (text
             ((@ (array
                  (or (@ *editor* mode) "")
                  (or (@ *editor* loaded-image-url) "")
                  (or (and (not (= zoom 1)) (+ "[" (round (* zoom 100)) "%]")) "")
                  (or (when-let (crop (@ *editor* crop))
                        (when (@ crop visible)
                          (+ "(" (nround (- (left crop) (left image)))
                             "+" (width crop)
                             "," (nround (- (top crop) (top image)))
                             "+" (height crop) ")")))
                      ""))
                 join) " ")))
      (set-html (@ *editor* modeline) text)))

  (defun create-image-cropper ()
    (let ((crop (rect fill "transparent"
                      stroke "red"
                      stroke-dash-array (array 2 -2)
                      visible false
                      border-color "white"
                      corner-color "blue"
                      corner-size 20
                      has-rotating-point false)))
      (setf (@ *editor* crop) crop)
      (on crop "moving" (update-modeline))
      (on crop "scaling" (update-modeline))
      crop))

  (defun create-controls (parent)
    (create-el ("div" parent)
               ((:paper-icon-button :icon "image:crop" :onclick "selectCropping()"))
               ((:paper-icon-button :icon "pan-tool" :onclick "selectPanning()"))))

  (defun select-cropping ()
    (setf (@ *editor* mode) :cropping
          (@ *editor* default-cursor) "crosshair")
    (update-modeline))

  (defun select-panning ()
    (setf (@ *editor* mode) :panning
          (@ *editor* default-cursor) "move")
    (update-modeline))

  (defun end-mode ()
    (setf (@ *editor* mode) nil
          (@ *editor* default-cursor) "default"
          (@ *editor* mouse-down-event) nil)
    (update-modeline))

  (defun middle-clickp (event)
    (= (@ event e button) 1))

  (defun setup-image-editor-events (editor)
    (on editor "mouse:wheel"
        (let* ((delta (@ e wheel-delta))
               (curzoom ((@ editor get-zoom)))
               (newzoom (+ curzoom (/ delta 4000)))
               (x (@ e offset-x))
               (y (@ e offset-y)))
          ((@ editor zoom-to-point) (create :x x :y y) newzoom)
          ((@ e prevent-default)))
        (update-modeline))
    (on editor "mouse:down"
        (with-fabric editor
          (cond
            ((middle-clickp event)
             (setf (@ editor mode) :panning
                   (@ editor mouse-down-event) e))
            ((eql (@ editor mode) :panning)
             (setf (@ editor mouse-down-event) e))
            ((eql (@ editor mode) :cropping)
             (let ((left (- (@ e page-x) (left (@ editor bounds))))
                   (top (- (@ e page-y) (top (@ editor bounds))))
                   (crop (@ editor crop)))
               (when (or (not (@ crop visible))
                         (not ((@ crop contains-point) (create :x left :y top))))
                 (setf (width crop) 2
                       (height crop) 2
                       (left crop) left
                       (top crop) top
                       (@ crop visible) t)
                 (setf (@ editor mouse-down-event) e)
                 (bring-to-front crop)))))))
    (on editor "mouse:up" (end-mode))
    (on editor "mouse:move"
        (when (and (@ editor mode) (@ editor mouse-down-event))
          (let ((mode (@ editor mode))
                (mde (@ editor mouse-down-event)))
            (cond
              ((eql mode :panning)
               (let* ((dx (- (@ e page-x) (@ mde page-x)))
                      (dy (- (@ e page-y) (@ mde page-y))))
                 ((@ editor relative-pan) (create :x dx :y dy))
                 (setf (@ editor mouse-down-event) e)))
              ((eql mode :cropping)
               (let ((crop (@ editor crop)))
                 (setf (width crop) (- (@ e page-x) (@ mde page-x))
                       (height crop) (- (@ e page-y) (@ mde page-y)))
                 ((@ crop set-coords))))))
          (update-modeline)
          ((@ editor render-all)))))

  (defun initialize-image-editor (canvas-id &optional controls-id modeline-id)
    (when controls-id
      (create-controls (id controls-id)))
    (setf *editor* (initialize-fabric canvas-id)
          (@ *editor* controls) (and controls-id (id controls-id))
          (@ *editor* mode) nil)
    (when modeline-id (setf (@ *editor* modeline) (id modeline-id)))
    (with-fabric *editor*
      (add (create-image-cropper)))
    (setup-image-editor-events *editor*))

  (defun load-image (url)
    (with-image (img url)
      (with-fabric *editor*
        ((@ img set) (create selectable false))
        (add img)
        (setf (@ *editor* loaded-image-url) url
              (@ *editor* image) img)
        (update-modeline)))))



