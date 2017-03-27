(in-package :FOO)

(defun FOO-file (&optional base)
  (directory-namestring (asdf:system-relative-pathname :FOO base)))


