(in-package :story)

(defparameter *production* nil)

(defun toggle-production ()
  (setf *production* (not *production*))
  (when *story* (story (name *story*)))
  (note ";; ~A in production." (if *production* "Now" "Not")))
