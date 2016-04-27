(in-package :story)

(defgeneric render (element stream)
  (:method :before ((element element) stream)
           (note "rendering ~S" element))
  (:method ((element element) stream)
    (iter (for child in (children element))
          (render child stream)))
  (:method ((page page) stream)
    (funcall (renderer page) page stream)
    (call-next-method)))

(defun render-stylesheets (story stream))

(defmethod render-complete-page ((page page) stream)
  (let* ((story (parent page))
         (title (or (title page) (title story))))
    (with-html-output (stream stream)
      (:html
        (:head
         (fmt "~%<!-- ~A ~A ~A -->~%" (name (parent page)) (git-latest-commit) (format-timestring nil (now)))
         (when title (htm (:title (esc title))))
         (when (stylesheets story) (render-stylesheets story stream)))
        (:body (funcall (body page) stream page))))))


