(in-package :story)

(defparameter *sample-images-font* (story-modules-file "roboto/fonts/Roboto-Medium.ttf"))

(define-story-module sample-images
    :dispatches ((:prefix "/sample-images/" serve-sample-image)))

(defun create-sample-image (stream text)
  (vecto:with-canvas (:width 400 :height 200)
    (let ((font (vecto:get-font *sample-images-font*)))
      (vecto:set-font font 40)
      (vecto:draw-centered-string 200 100 text)
      (vecto:save-png-stream stream))))

(defun serve-sample-image ()
  (let ((request-path (script-name*)) served)
    (setf (content-type*) "image/png")
    (let ((out (send-headers)))
      (create-sample-image out (pathname-name request-path)))))
