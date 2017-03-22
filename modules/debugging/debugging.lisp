(in-package :story)

(define-story-module debugging
  :scripts (("debug.js" debugging-js))
  :files (("debug.json" debugging-information))
  :depends-on (:iron-request))

(defun debugging-information ()
  (to-json (server-info)))

(in-package :story-js)

(define-script debugging-js

  (defun fetch-debugging-info (callback)
    (request "/debug.json"
             (lambda (val)
               (funcall callback (eval (+ "(" (@ val response) ")"))))))

  (defun render-debugging-info (container)
    (fetch-debugging-info
     (lambda (info)
       (console info)
       (let* ((div (id container))
              (parent (create-element "table" div "debugging-info")))
         (for-in (slot info)
                 (create-el-html* ("tr" parent)
                                  (:td slot)
                                  (:td
                                   (let ((data (getprop info slot)))
                                     (if (arrayp data)
                                         ((@ data join) "<br>")
                                         data))))))))))


