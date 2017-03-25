(in-package :story)

(define-demo editor ((:editor))
  (:div :id "editor" :style "width:600px;height:500px;"
        (fmt "~A~%~%~A~%" "Hello! Edit me!"
             (as-string (fractal 186 3 3 :char #\black_diamond))))
  (script
    (defvar *editor* ((@ ace edit) "editor"))))
