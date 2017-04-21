(in-package :story)

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

(export '(dom-repeat template-grid template-grid-template))
