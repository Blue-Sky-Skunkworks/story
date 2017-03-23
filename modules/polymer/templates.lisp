(in-package :story)

(defmacro define-template (name &key style properties content)
  (let ((sname (format nil "~(~A~)" name))
        (props (loop for (name type value) in properties
                     appending `(,name (create type ,(string-capitalize type)
                                               ,@(when value `(value ,value)))))))
    `(defun ,(symb name '-template) ()
       (html-to-string
         (:dom-module :id ,sname
                      (:template
                       ,@(when style
                           `((:style (str (cl-css:css ',style)))))
                       ,@content))
         (:script
           (str ,(ps* `(*polymer (create is ,sname properties (create ,@props))))))))))

(defmacro dom-repeat (&body body) `(html (:template :is "dom-repeat" ,@body)))

(export '(define-template dom-repeat))





