(in-package :story)

(define-demo quicklisp ((:quicklisp))
  (:div :id "quicklisp")
  (script (render-quicklisp-listing "quicklisp" "ql.json")))

