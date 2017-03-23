(in-package :story)

(defmacro define-template (name &key style properties content methods)
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
           (str ,(ps* `(*polymer (create is ,sname properties (create ,@props)
                                         ,@methods)))))))))

(defmacro dom-repeat (&body body) `(html (:template :is "dom-repeat" ,@body)))

(define-template template-grid
  :properties (("source" string)
               ("renderer" object))
  :content
  ((ajax :auto t :url "{{source}}" :handle-as "json" :on-response "handleResponse"))
  :methods
  (default-grid-renderer
   (lambda (el)
     (story-js::set-html (create-el "table")
                         (ps:loop :for n :of el
                            collect (htm (:tr (:th n) (:td (aref el n)))))))
   handle-response
   (lambda (resp)
     (let ((renderer (or (@ this renderer) (@ this default-grid-renderer))))
       (when (stringp renderer) (setf renderer (function-from-string renderer)))
       (loop for el in (@ resp detail response)
             do ((@ this append-child) (funcall renderer el)))))))

(export '(define-template dom-repeat template-grid))





