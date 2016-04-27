(in-package :story)

(defvar *loaded-story-modules* nil)

(defmacro when-module (name &body body)
  `(when (member ,name *story-modules*) ,@body))

(defmacro define-story-module (name &rest args)
  (with-assoc-values (args (:init))
    `(progn
       (defun ,(symb 'load-story-module- name) (&key force)
         (when (member ,(ksymb (string-upcase name)) *loaded-story-modules*)
           (if force
               (warn ,(format nil  "Reinitializing story module ~S." name))
               (error ,(format nil "Story module ~S already loaded." name))))
         ,@init
         (prog1
            nil
           (pushnew ,(ksymb (string-upcase name)) *loaded-story-modules*))))))

(defun story-module-depends-on-modules (module-name)
  (iter (for name in (asdf:system-depends-on (asdf:find-system module-name)))
        (when (string-starts-with name "story-module-")
          (appending (story-module-depends-on-modules name))
          (collect (ksymb (string-upcase (subseq name (length "story-module-"))))))))


(defun list-story-modules (&key with-version)
  (iter (for system in (ql:list-local-systems))
        (when (and (string-starts-with system "story-module-")
                   (not (equal system "story-module-system")))
          (collect
              (if with-version
                  (list (subseq system 13)
                        (parse-float (asdf:component-version (asdf:find-system system))))
                  (subseq system 13))))))

(defun ensure-story-module (name)
  (unless (member (ksymb (string-upcase name)) *loaded-story-modules*)
    (require (symb 'story-module- (string-upcase name)))
    (funcall (symb 'load-story-module- (string-upcase name)))))
