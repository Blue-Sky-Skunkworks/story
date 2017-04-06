(in-package :story)

(defmacro define-template (name &key style properties content methods)
  (let* ((method-names (iter (for method in methods) (collect (car method))))
         (methods
           (iter (for method in methods)
             (collect (cons (car method)
                            (let ((rtn (cdr method)))
                              (iter (for method-name in method-names)
                                (setf rtn (nsubst `(@ this ,method-name) method-name rtn)))
                              rtn))))))
    (let ((sname (format nil "~(~A~)" name))
          (props (loop for (name type value) in properties
                       appending
                       `(,name (create type ,(string-capitalize type)
                                       ,@(when value `(value ,(if (symbolp value)
                                                                  (symbol-value value) value))))))))
      `(defun ,(symb name '-template) ()
         (html-to-string
           (:dom-module :id ,sname
                        (:template
                         ,@(when style
                             `((:style (str (cl-css:css ',style)))))
                         ,@content))
           (:script
             (str ,(ps* `(*polymer (create is ,sname properties (create ,@props)
                                           ,@(iter (for (name args . body) in methods)
                                               (appending (list name `(lambda ,args ,@body))))))))))))))

(defmacro dom-repeat (&body body) `(html (:template :is "dom-repeat" ,@body)))

(define-template template-grid
  :style ((":host" :display block))
  :properties (("source" string)
               ("renderer" object))
  :content
  ((ajax :auto t :url "{{source}}" :handle-as "json" :on-response "handleResponse"))
  :methods
  ((default-grid-renderer (el)
     (story-js::set-html (create-el "table")
                         (ps:loop :for n :of el
                            collect (htm (:tr (:th n) (:td (aref el n)))))))

   (handle-response (resp)
     (let ((renderer (or (@ this renderer) (@ this default-grid-renderer))))
       (when (stringp renderer) (setf renderer (function-from-string renderer)))
       (loop for data in (@ resp detail response)
             do (let ((el (funcall renderer data)))
                  ((@ this append-child) el)))))))

(export '(define-template dom-repeat template-grid template-grid-template))





