(in-package :story)

(define-demo files ((:files) :directories (("modules/demo-files" "files")))
  (:div :id "files")
  (script (render-file-listing "files" "/files/")))

(defun create-demo-file-listing ()
  (save-file-listing (story-file "modules/demo-files/")))


