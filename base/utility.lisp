(in-package :FOO)

(defun FOO-file (&optional base)
  (namestring (asdf:system-relative-pathname :FOO base)))


