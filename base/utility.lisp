(in-package :FOO)

(defun asdf-base-path (name)
  (directory-namestring (asdf:component-pathname (asdf:find-system name))))

(defun FOO-file (&optional  base)
  (concatenate 'string (asdf-base-path :FOO) base))


