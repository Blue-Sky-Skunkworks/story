(in-package :story)

(defvar *demos* nil)

(defmacro define-demo (name (&optional modules &key dispatches directories stylesheets) &body body)
  (let ((title (format nil "Story Demo ~A" (string-capitalize name))))
    `(progn
       (pushnew ',name *demos*)
       (define-story ,(symb 'demo- name) (:title ,title :modules ,modules
                                          :dispatches ,dispatches
                                          :directories ,directories
                                          :stylesheets ,stylesheets)
        (:h1 :style "font-family:sans-serif;" ,title)
        ,@body))))

;; (define-demo trivial ())


