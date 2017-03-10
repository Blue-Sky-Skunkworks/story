(in-package :story)

(define-demo fabric ((:fabric))
  (:button :onclick (ps (hitme)) "testme")
  (:canvas :width 200 :height 200 :id "c")
  (script
    (defvar *c* (new ((@ fabric *canvas) "c")))
    (defvar *r* (new ((@ fabric *rect)
                      (create :left 100
                              :top 100
                              :fill "red"
                              :width 20
                              :height 20
                              :angle 45))))
    ((@ *c* add) *r*)
    (defun hitme ()
      ((@ *r* set) (create :left (* ((@ *math random)) 200)
                           :top (* ((@ *math random)) 200)))
      ((@ *c* render-all)))))

