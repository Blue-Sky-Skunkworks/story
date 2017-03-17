(in-package :story)

(define-demo debugging ((:debugging))
  (:div :id "deb")
  (script (render-debugging-info "deb")))

