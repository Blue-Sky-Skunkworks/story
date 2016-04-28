(in-package :story)

(defvar *story-modules* (make-hash-table))

(defvar *loaded-story-modules* nil)

(defun story-modules ()
  "Print a table of the story modules."
  (iter (for (k v) in-hashtable *story-modules*)
        (format t "~A ~A~20T ~S~%" (if (member k *loaded-story-modules*) "*" " ") k v)))

(defun module-value (name key)
  (assoc-value (gethash name *story-modules*) key))

(defun collect-module-value (key &rest modules)
  (iter (for module in modules)
        (appending (module-value module key))))

(defun collect-module-stylesheets (modules)
  (iter (for module in modules)
        (when-let (els (module-value module :stylesheets))
          (appending
           (iter (for css in els) (collect (format nil "~(~A~)/~A.css" module (pathname-name css))))))))

(defmacro when-module (name &body body)
  `(when (member ,name *story-modules*) ,@body))

(defmacro define-story-module (name &key init stylesheets)
  (let ((kname (ksymb (string-upcase name))))
    `(progn
       (setf (gethash ,kname *story-modules*)
             (list ,@(when stylesheets `((list :stylesheets ,@stylesheets)))))
       (defun ,(symb 'load-story-module- name) (&key force)
         (when (member ,kname *loaded-story-modules*)
           (if force
               (warn ,(format nil  "Reinitializing story module ~S." name))
               (error ,(format nil "Story module ~S already loaded." name))))
         ,@(when stylesheets `((load-stylesheets
                                ,@(iter (for css in stylesheets)
                                        (appending
                                            (list
                                             (format nil "~A~(~A~)/~A" (story-modules-file) name css)
                                             (format nil "css/~(~A~)/~A.css" name (pathname-name css))))))))
         ,@init
         (prog1
             nil
           (pushnew ,kname *loaded-story-modules*))))))

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

(defun ensure-story-modules (names)
  (mapc #'ensure-story-module names))
