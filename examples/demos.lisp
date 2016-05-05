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
  (script (setup-packing "pack" "el")))

(defun load-computer-calender ()
  (iter (for line in
             (split-sequence #\newline
                             (slurp-file "/usr/share/calendar/calendar.computer")))
        (multiple-value-bind (ms me rs re) (scan "^(\\d+?)/(\\d+)\\t(.+, ([0-9]{4}).*)$" line)
          (when ms
            (collect
                (list
                 (parse-integer (subseq line (aref rs 0) (aref re 0)))
                 (parse-integer (subseq line (aref rs 1) (aref re 1)))
                 (parse-integer (subseq line (aref rs 3) (aref re 3)))
                 (concatenate 'string (subseq line (aref rs 2) (- (aref rs 3) 2))
                              (subseq line (aref re 3) (aref re 2)))))))))

(define-demo timeline (:timeline)
  (:div :id "vis")
  (:script
    (str (ps* `(progn
                 (new ((@ vis *timeline)
                       (story-js:id "vis")
                       (new ((@ vis *data-set)
                             (make-array
                              ,@(iter (for (y m d text) in (load-computer-calender))
                                      (for index from 1)
                                      (collect `(create :id ,index :content ,text :start ,(format nil "~A-~A-~A" y m d))))))))))))))
