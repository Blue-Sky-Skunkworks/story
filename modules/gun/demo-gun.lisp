(in-package :story)

(define-demo gun ((:gun))
  (script
    (defvar *gun*)

    (defun initialize-gun ()
      (setf *gun* (*gun))
      (console "gun initialized"))

    (defun add-gun (k v)
      ((@ ((@ *gun* get) k) put) v))

    (initialize-gun)

    )
  )

