(in-package :story)

(define-demo files ((:files) :directories (("modules/demo-images" "images")))
  (:div :id "files")
  (script
    (render-file-listing "files" "images" :headings t)))

(defun create-demo-file-listing ()
  (save-file-listing (story-file "modules/demo-images/")))


