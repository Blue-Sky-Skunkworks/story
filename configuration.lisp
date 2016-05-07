(in-package :story)

(defparameter *production* nil)

(defun toggle-production ()
  (setf *production* (not *production*))
  (note ";; ~A in production." (if *production* "Now" "Not")))
