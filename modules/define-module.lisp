(in-package :story)

(defvar *story-modules* (make-hash-table))

(defvar *loaded-story-modules* nil)

(defclass module ()
  ((name :reader name :initarg :name)
   (stylesheets :reader stylesheets :initarg :stylesheets)
   (directories :reader directories :initarg :directories)
   (scripts :reader scripts :initarg :scripts)))

(defmethod print-object ((module module) stream)
  (print-unreadable-object (module stream :type t)
    (format stream "~A" (name module))))

(defun story-modules ()
  "Print a table of the story modules."
  (iter (for (k v) in-hashtable *story-modules*)
        (format t "~A ~A~%" (if (member k *loaded-story-modules*) "*" " ") v)))

(defun collect-module-stylesheets (modules)
  (iter (for module in modules)
        (when-let (els (stylesheets (gethash module *story-modules*)))
          (appending
           (iter (for css in els) (collect (format nil "~(~A~)/~A.css" module (pathname-name css))))))))

(defmacro when-module (name &body body)
  `(when (member ,name *story-modules*) ,@body))

(defmacro define-story-module (name &key init stylesheets directories scripts)
  (let ((kname (ksymb (string-upcase name))))
    `(progn
       (setf (gethash ,kname *story-modules*)
             (make-instance 'module :name ,kname :stylesheets ',stylesheets :directories ',directories :scripts ',scripts))
       (defun ,(symb 'load-story-module- name) ()
         (when (member ,kname *loaded-story-modules*)
           (warn ,(format nil  "Reinitializing story module ~S." name)))
         ,@(when stylesheets `((load-stylesheets
                                ,@(iter (for css in stylesheets)
                                        (appending
                                            (list
                                             (format nil "~A~(~A~)/~A" (story-modules-file) name css)
                                             (format nil "/css/~(~A~)/~A.css" name (pathname-name css))))))))
         ,@(when scripts `((load-scripts ,@(iter (for script in scripts)
                                                 (appending
                                                  (list
                                                   (format nil "~A~(~A~)/~A" (story-modules-file) name script)
                                                   (format nil "/js/~(~A~)/~A.~A" name (pathname-name script) (pathname-type script))))))))
         ,@(when directories `((load-directories ,@(iter (for (from to) in directories)
                                                         (appending
                                                          (list
                                                           (format nil "~A~(~A~)/~A/" (story-modules-file) name from)
                                                           (format nil "/~A/" to)))))))
         ,@init
         (pushnew ,kname *loaded-story-modules*)
         (values)))))

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
    (require (symb 'story-module- (string-upcase name))))
  (funcall (symb 'load-story-module- (string-upcase name))))

(defun ensure-story-modules (names)
  (mapc #'ensure-story-module names))
