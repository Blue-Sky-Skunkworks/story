(in-package :story)

(defgeneric additional-file-information (type file)
  (:method (type file) (warn "Unhandled file type ~S." type))
  (:method ((type (eql :text/html)) file)
    (let ((title (story-parsing::page-title (slurp-file file))))
      (when title
        (list (cons :comment title)))))
  (:method ((type (eql :image/png)) file)
    (multiple-value-bind (w h) (png-image-size file)
      (nconc (list (cons :width w) (cons :height h)
                   (cons :thumbnail (create-image-thumbnail file)))
             (when-let (comment (image-comment file)) (list (cons :comment comment))))))
  (:method ((type (eql :image/jpeg)) file)
    (multiple-value-bind (w h) (jpeg-image-size file)
      (nconc (list (cons :width w) (cons :height h)
                   (cons :thumbnail (create-image-thumbnail file)))
             (when-let (comment (image-comment file)) (list (cons :comment comment)))))))


