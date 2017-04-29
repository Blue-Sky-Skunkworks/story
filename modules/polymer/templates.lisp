(in-package :story)

(defvar *templates* (make-hash-table))

(defmacro define-template (name &key style properties content methods listeners)
  `(progn
     (setf (gethash ',name *templates*) (list ',style ',properties ',content ',methods
                                              (html-to-string ,@content) ',listeners))
     (defun ,(symb name '-template) () (create-template ',name))))

(defvar *template-methods* (make-hash-table))

(defmacro define-template-method (template name args &body body)
  (when (eq (caar body) 'sb-int:quasiquote)
    (setf (car body) (eval (car body))))
  (when-let (hit (gethash template *template-methods*))
    (setf (gethash template *template-methods*)
          (remove name hit :key #'car)))
  (push `(,name ,args ,@body) (gethash template *template-methods*))
  nil)

(defun template-methods (name) (gethash name *template-methods*))

(defun create-template (name)
  (destructuring-bind (style properties content methods content-html listeners)
      (gethash name *templates*)
    (declare (ignore content))
    (let* ((methods (append methods (template-methods name)))
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
                        listeners (create ,@(iter (for (name fn) in listeners)
                                              (appending (list name fn))))
                        ,@(iter (for (name args . body) in methods)
                            (appending
                             (list name
                                   `(lambda ,args
                                      (let ((that this))
                                        (macrolet ,(iter (for name in method-names)
                                                     (collect `(,name (&rest args)
                                                                      (cons (list '@ 'that ',name)
                                                                            args))))
                                         ,@body))))))))))))))))

(export '(define-template define-template-method))
