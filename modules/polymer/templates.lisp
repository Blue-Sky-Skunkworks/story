(in-package :story)

(defvar *templates* (make-hash-table))

(defmacro define-template (name &key style properties content methods)
  `(progn
     (setf (gethash ',name *templates*) (list ',style ',properties ',content ',methods
                                              (html-to-string ,@content)))
     (defun ,(symb name '-template) () (create-template ',name))))

(defvar *template-methods* (make-hash-table))

(defmacro define-template-method (name template args &body body)
  (when-let (hit (gethash template *template-methods*))
    (setf (gethash template *template-methods*)
          (remove name hit :key #'car)))
  (push (list name args body) (gethash template *template-methods*))
  nil)

(defun template-methods (name) (gethash name *template-methods*))

(defun create-template (name)
  (destructuring-bind (style properties content methods content-html) (gethash name *templates*)
    (declare (ignore content))
    (let ((methods (append methods (template-methods name)))
          (method-names (iter (for method in methods) (collect (car method)))))
      (let ((sname (format nil "~(~A~)" name))
            (props (loop for (name type value) in properties
                         appending
                         `(,name (create type ,(string-capitalize type)
                                         ,@(when value `(value ,(if (symbolp value)
                                                                    (symbol-value value) value))))))))
        (html-to-string
          (:dom-module :id sname
                       (:template
                        (when style (htm (:style (str (cl-css:css style)))))
                        (str content-html)))
          (:script
            (str
             (ps*
              `(*polymer
                (create is ,sname properties (create ,@props)
                        ,@(iter (for (name args . body) in methods)
                            (appending
                             (list name
                                   `(lambda ,args
                                      (macrolet ,(iter (for name in method-names)
                                                   (collect `(,name (&rest args)
                                                                    (cons (list '@ 'this ',name)
                                                                          args))))
                                        ,@body)))))))))))))))


(defmacro define-template (name &key style properties content methods)
  (let ((methods (append methods (template-methods name)))
        (method-names (iter (for method in methods) (collect (car method)))))
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
             (str
              ,(ps*
                `(*polymer
                  (create is ,sname properties (create ,@props)
                          ,@(iter (for (name args . body) in methods)
                              (appending
                               (list name
                                     `(lambda ,args
                                        (macrolet ,(iter (for name in method-names)
                                                     (collect `(,name (&rest args)
                                                                      (cons (list '@ 'this ',name)
                                                                            args))))
                                          ,@body)))))))))))))))

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





