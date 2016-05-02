(in-package :story)

(defmacro define-demo (name modules &body body)
  (let ((title (format nil "Story Demo ~A" (string-capitalize name))))
    `(define-story ,(symb 'demo- name) (:title ,title :modules ,modules)
       (:h1 ,title)
       ,@body)))

(define-demo trivial ())

(define-demo roboto (:roboto))

(define-demo packery (:packery)
  (:div :id "pack"
        (iter (for x from 1 to 10)
              (htm (:div :class "el" :style "width:200;height:200;background:blue;"))))
  (:script (str (ps (setup-packing "pack" "el")))))
