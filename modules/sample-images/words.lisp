(in-package :story)

(defvar *words* nil)

(defun load-words ()
  (setf *words*
        (coerce
         (with-open-file (f "/usr/share/dict/words"
                            :direction :input :if-does-not-exist :error)
           (iter (for line = (read-line f nil))
                 (while line)
                 (collect line)))
         'vector))
  (length *words*))

(defun random-word ()
  (unless *words* (load-words))
  (aref *words* (random (length *words*))))
