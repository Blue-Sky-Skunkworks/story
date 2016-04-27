(in-package :story)

(defvar *story-modules* nil)

(defmacro when-module (name &body body)
  `(when (member ,name *story-modules*) ,@body))

(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

(defmacro progress-every (count fmt &rest args)
  `(when (and progress (zerop (mod index ,count)))
     (funcall progress (format nil ,fmt ,@args))))

(defmacro define-story-module (name &rest args)
  (with-assoc-values (args (:init))
    `(progn
       (defun ,(symb 'load-story-module- name) (&key force)
         (setf %module-id module-id)
         (when (member ,(ksymb (string-upcase name)) *story-modules*)
           (if force
               (warn "Reinitializing story module ~S." ',name)
               (error "Story module ~S already loaded." ',name)))
         ,@init
         (prog1
            nil
           (pushnew ,(ksymb (string-upcase name)) *story-modules*))))))

(defun story-module-depends-on-modules (module-name)
  (iter (for name in (asdf:system-depends-on (asdf:find-system module-name)))
        (when (string-starts-with name "story-module-")
          (appending (story-module-depends-on-modules name))
          (collect (ksymb (string-upcase (subseq name (length "story-module-"))))))))
