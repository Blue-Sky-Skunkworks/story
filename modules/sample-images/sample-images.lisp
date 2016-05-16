(in-package :story)

(defparameter *sample-images-font* (story-modules-file "roboto/fonts/Roboto-Medium.ttf"))
(defparameter *sample-image-width* 400)
(defparameter *sample-image-height* 200)
(defparameter *sample-image-font-size* 40)

(define-story-module sample-images
  :dispatches ((:prefix "/sample-images/" serve-sample-image)
               (:prefix "/sample-images-random/" serve-sample-image-random)))

(defun create-sample-image (stream text)
  (vecto:with-canvas (:width *sample-image-width* :height *sample-image-height*)
    (let ((font (vecto:get-font *sample-images-font*)))
      (vecto:set-font font *sample-image-font-size*)
      (vecto:draw-centered-string (round *sample-image-width* 2) (round *sample-image-height* 2) text)
      (vecto:save-png-stream stream))))

(defun serve-sample-image ()
  (setf (content-type*) "image/png")
  (create-sample-image (send-headers) (pathname-name (script-name*))))

(defun serve-sample-image-random ()
  (setf (content-type*) "image/png")
  (create-sample-image (send-headers) (random-word)))

