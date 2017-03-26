(in-package :story)

(define-demo gun ((:gun))
  (script
    (defvar *gun*)

    (defun initialize-gun ()
      (setf *gun* (*gun)))

    (initialize-gun)))

