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

(defun render-stylesheets (story stream)
  (html
    (iter (for css in (stylesheets story))
          (htm (:link :rel "stylesheet" :type "text/css" :href (format nil "css/~A" css))))))

(defun render-scripts (story stream)
  (html
    (iter (for script in (scripts story))
          (htm (:script :type "text/javascript" :src (format nil "js/~A" script))))))

(defmethod render-complete-page ((page page) stream)
  (let* ((story (parent page))
         (production (production story))
         (title (or (title page) (title story))))
    (with-html-output (stream stream)
      (:html
        (:head
         (fmt "~%<!-- ~A ~A ~A -->~%" (name (parent page)) (git-latest-commit) (format-timestring nil (now)))
         (when title (htm (:title (esc title))))
         (cond
           (production
            (when (stylesheets story) (htm (:link :rel "stylesheet" :type "text/css" :href "css/css-all.css")))
            (when (scripts story) (htm (:script :type "text/javascript" :src "js/js-all.js"))))
           (t
            (when (stylesheets story) (render-stylesheets story stream))
            (htm (:script :type "text/javascript" :src "js/js-all.js"))
            (when (scripts story) (render-scripts story stream)))))
        (:body (funcall (body page) stream page))))))


