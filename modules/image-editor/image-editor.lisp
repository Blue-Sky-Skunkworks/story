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
    (let ((text
            (+
             (or (+ (@ *editor* loaded-image-url) " ") "")
             (or (when-let (crop (@ *editor* crop))
                   (when (@ crop visible)
                     (+ "(" (nround (left crop))
                        "+" (width crop)
                        "," (nround (top crop))
                        "+" (height crop) ")")))
                 ""))))
      (set-html (@ *editor* modeline) text)))

  (defun create-image-cropper ()
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

  (defun create-controls (parent)
    (create-el ("div" parent)
               ((:paper-icon-button :icon "image:crop")))
    )

  (defun initialize-image-editor (canvas-id &optional controls-id modeline-id)
    (when controls-id
      (create-controls (id controls-id)))
    (setf *editor* (initialize-fabric canvas-id)
          (@ *editor* controls) (and controls-id (id controls-id)))
    (when modeline-id (setf (@ *editor* modeline) (id modeline-id)))
    (with-fabric *editor*
      (add (create-image-cropper)))
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
              (setf (@ *editor* mouse-down-event) (@ event e))
              (bring-to-front crop)))))
    (on *editor* "mouse:up" (setf (@ *editor* mouse-down-event) nil))
    (on *editor* "mouse:move"
        (when (@ *editor* mouse-down-event)
          (let ((crop (@ *editor* crop))
                (mde (@ *editor* mouse-down-event)))
            (setf (width crop) (- (@ event e page-x) (@ mde page-x))
                  (height crop) (- (@ event e page-y) (@ mde page-y)))
            ((@ crop set-coords)))
          (update-modeline)
          ((@ *editor* render-all)))))

  (defun load-image (url)
    (with-image (img url)
      (with-fabric *editor*
        ((@ img set) (create selectable false))
        (add img)
        (setf (@ *editor* loaded-image-url) url)
        (update-modeline)))))



