(in-package :story)

(defun load-computer-calender ()
  (iter (for line in (split-sequence #\newline (slurp-file "/usr/share/calendar/calendar.computer")))
        (multiple-value-bind (ms me rs re) (scan "^(\\d+?)/(\\d+)\\t(.+, ([0-9]{4}).*)$" line)
          (declare (ignore me))
          (when ms
            (collect
                (list
                 (parse-integer (subseq line (aref rs 0) (aref re 0)))
                 (parse-integer (subseq line (aref rs 1) (aref re 1)))
                 (parse-integer (subseq line (aref rs 3) (aref re 3)))
                 (concatenate 'string (subseq line (aref rs 2) (- (aref rs 3) 2))
                              (subseq line (aref re 3) (aref re 2)))))))))

(define-demo timeline (:vis)
  (:div :id "id")
  (script*
    `(new ((@ vis *timeline)
           (id "id")
           (new ((@ vis *data-set)
                 (make-array
                  ,@(iter (for (y m d text) in (load-computer-calender))
                          (for index from 1)
                          (collect `(create :id ,index :content ,text :start ,(format nil "~A-~A-~A" y m d)))))))))))




