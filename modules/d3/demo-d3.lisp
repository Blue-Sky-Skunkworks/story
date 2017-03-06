(in-package :story)

(defpsmacro @@ (head &body body)
  (labels ((recur (head steps)
             (if steps
                 (recur `((@ ,head ,(caar steps)) ,@(cdar steps)) (cdr steps))
                 head)))
    (recur head body)))

(defpsmacro d3 (&body body)
  `(@@ d3 ,@body))

(define-demo d3 ((:d3) :stylesheets (("d3demo.css" d3demo-css)))
  (script
    (d3 (select "body") (append "p") (text "D3 Works!"))
    (defvar *dataset* (array 5 10 15 20 25))
    (d3 (select "body") (select-all "div") (data *dataset*)
      (enter) (append "div") (attr "class" "bar")
      (style "height" (lambda (d) (+ (* d 5) "px"))))
    (defvar *svg* (d3 (select "body") (append "svg") (attr "width" 500) (attr "height" 50)))
    (@@ *svg* (select-all "circle") (data *dataset*) (enter) (append "circle")
              (attr "cx" (lambda (d i) (+  (* i 50) 25)))
              (attr "cy" 25)
              (attr "r" (lambda (d) d)))))

(in-package :story-css)

(defun d3demo-css ()
  (css
   `(("div.bar" :display inline-block
                :height 75px
                :width 20px
                :background-color teal))))
