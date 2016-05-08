(in-package :story)

(defvar *story-modules* (make-hash-table))

(defun find-module (name)
  (or (gethash name *story-modules*)
      (error "Missing story module ~S." name)))

(defvar *loaded-story-modules* nil)

(defclass module ()
  ((name :reader name :initarg :name)
   (stylesheets :reader stylesheets :initarg :stylesheets)
   (directories :reader directories :initarg :directories)
   (scripts :reader scripts :initarg :scripts)
   (imports :reader imports :initarg :imports)
   (extends :reader extends :initarg :extends)
   (dispatches :reader dispatches :initarg :dispatches)
   (suffixes :reader suffixes :initarg :suffixes)
   (prefixes :reader prefixes :initarg :prefixes)))

(defmethod print-object ((module module) stream)
  (print-unreadable-object (module stream :type t)
    (format stream "~A" (name module))))

(defun story-modules ()
  "Print a table of the story modules."
  (let ((*print-pretty* nil))
    (iter (for (k v) in-hashtable *story-modules*)
          (format t "~A ~A~%~@[~30Tcss  ~{~S~^, ~}~%~]~@[~30Tdir  ~{~S~^, ~}~%~]~@[~30Tjs  ~{~S~^, ~}~%~]~@[~30Tin  ~{~S~^, ~}~%~]"
                  (if (member k *loaded-story-modules*) "*" " ")
                  (name v) (stylesheets v) (directories v) (scripts v) (imports v)))))

(defun modules-and-parents (modules)
  (remove-duplicates
   (iter (for module in modules)
         (when-let (extends (extends (find-module module)))
           (collect extends))
         (collect module))
   :from-end t))

(defun collect-module-imports (modules)
  (iter (for name in (modules-and-parents modules))
        (let ((module (find-module name)))
          (when-let (els (imports module))
            (appending
             (iter (for el in els) (collect (format nil "~(~A~)/~A.html" (or (extends module) name) el))))))))

(defun collect-module-stylesheets (modules)
  (iter (for name in (modules-and-parents modules))
        (let ((module (find-module name)))
         (when-let (els (stylesheets module))
           (appending els))))))

(defun collect-module-scripts (modules)
  (iter (for name in (modules-and-parents modules))
        (let ((module (find-module name)))
          (when-let (els (scripts module))
            (appending
             (iter (for script in els)
                   (collect
                       (if (and (stringp script) (char= (char script 0) #\/))
                           (subseq script 1)
                           (format nil "~(~A~)/~A" (or (extends module) name) (if (stringp script) script (first script)))))))))))

(defun collect-module-suffixes (modules)
  (iter (for name in (modules-and-parents modules))
        (let ((module (find-module name)))
          (when-let (els (suffixes module))
            (appending
             (iter (for suffix in els)
                   (collect (format nil "modules/~(~A~)/~A" (or (extends module) name) suffix))))))))

(defun collect-module-prefixes (modules)
  (iter (for name in (modules-and-parents modules))
        (let ((module (find-module name)))
          (when-let (els (prefixes module))
            (appending
             (iter (for prefix in els)
                   (collect (format nil "modules/~(~A~)/~A" (or (extends module) name) prefix))))))))

(defmacro when-module (name &body body)
  `(when (member ,name *story-modules*) ,@body))

(defun ensure-css-extension (path)
  (cond
    ((not (equalp (pathname-type path) "css"))
     (format nil "~Acss" (subseq path 0 (- (length path) (length (pathname-type path))))))
    (t path)))

(defmacro define-story-module (name &key init stylesheets directories scripts imports extends dispatches suffixes prefixes)
  (let ((kname (ksymb (string-upcase name)))
        (mname (or extends name)))
    `(progn
       (setf (gethash ,kname *story-modules*)
             (make-instance 'module :name ,kname :stylesheets ',stylesheets
                            :directories ',directories :scripts ',scripts :imports ',imports
                            :extends ,extends :dispatches ',dispatches
                            :suffixes ',suffixes :prefixes ',prefixes))
       (defun ,(symb 'load-story-module- name) ()
         ,@(when extends `((,(symb 'load-story-module- extends))))
         (when (member ,kname *loaded-story-modules*)
           (warn ,(format nil  "Reinitializing story module ~S." name)))
         ,@(when stylesheets `((load-stylesheets
                                ,@(iter (for css in stylesheets)
                                        (appending
                                         (list
                                          (format nil "~A~(~A~)~A" (story-modules-file) mname css)
                                          (ensure-css-extension css)))))))
         ,@(when scripts `((load-scripts ',(iter (for script in scripts)
                                                 (cond
                                                   ((stringp script)
                                                    (if (char= (char script 0) #\/)
                                                        (appending
                                                         (list
                                                          (format nil "~A~(~A~)~A" (story-modules-file) mname script)
                                                          script))
                                                        (appending
                                                         (list
                                                          (format nil "~A~(~A~)/~A" (story-modules-file) mname script)
                                                          (format nil "/~(~A~)/~A.~A" mname (pathname-name script) (pathname-type script))))))
                                                   (t (appending
                                                       (list
                                                        (intern (symbol-name (second script)) :story-js)
                                                        (format nil "/~(~A~)/~A" mname (first script))))))))))
         ,@(when directories `((load-directories ,@(iter (for dir in directories)
                                                         (let ((from (if (consp dir) (first dir) dir))
                                                               (to (if (consp dir) (second dir) dir)))
                                                           (appending
                                                            (list
                                                             (format nil "~A~(~A~)/~A/" (story-modules-file) mname from)
                                                             (format nil "/~A/" to))))))))
         ,@(when imports `((load-imports ',(iter (for el in imports)
                                                 (collect (format nil "~A~(~A~)/imports/~A.html" (story-modules-file) mname el))))))
         ,@(when dispatches `((load-module-dispatches ',dispatches)))
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
  ;; (unless (member (ksymb (string-upcase name)) *loaded-story-modules*)
  ;;   (require (symb 'story-module- (string-upcase name))))
  (funcall (symb 'load-story-module- (string-upcase name))))
