(in-package :story)

(defmacro define-demo (name modules &body body)
  (let ((title (format nil "Story Demo ~A" (string-capitalize name))))
    `(define-story ,(symb 'demo- name) (:title ,title :modules ,modules)
       (:h1 ,title)
       ,@body)))

(define-demo trivial ())

