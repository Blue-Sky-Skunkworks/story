(in-package :story)

(define-demo image-gallery ((:image-gallery) :directories (("modules/demo-images" "images")))
  (:div :id "images")
  (script
    (render-image-gallery "images" "/images/")))

(defun create-demo-image-gallery-listing ()
  (save-file-listing (story-file "modules/demo-images/")))
