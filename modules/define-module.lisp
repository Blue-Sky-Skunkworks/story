(in-package :story)

(defvar *story-modules* (make-hash-table))

(defun find-module (name &key (errorp t))
  (or (gethash name *story-modules*)
      (when errorp (error "Missing story module ~S." name))))

(defmethod print-object ((module module) stream)
  (print-unreadable-object (module stream :type t)
    (format stream "~A" (name module))))

(defun modules-and-parents (modules)
  (remove-duplicates
   (iter (for module in modules)
         (when-let (extends (extends (find-module module)))
           (collect extends))
         (collect module))
   :from-end t))

(defmacro when-module (name &body body)
  `(when (member ,name *story-modules*) ,@body))

(defun ensure-css-extension (path)
  (cond
    ((not (equalp (pathname-type path) "css"))
     (format nil "~Acss" (subseq path 0 (- (length path) (length (pathname-type path))))))
    (t path)))

(defmacro define-story-module (name &key init stylesheets directories scripts
                                      imports production-import-fix
                                      extends dispatches suffixes prefixes files)
  (let* ((kname (ksymb (string-upcase name)))
         (mname (or extends name))
         (base (format nil "~A~(~A~)/" (story-modules-file) mname)))
    `(progn
       (setf (gethash ,kname *story-modules*)
             (make-instance 'module :name ,kname :stylesheets ',stylesheets
                                    :directories ',directories :scripts ',scripts
                                    :imports ',imports :production-import-fix ',production-import-fix
                                    :extends ,extends :dispatches ',dispatches
                                    :suffixes ',suffixes :prefixes ',prefixes :files ',files))
       (defun ,(symb 'stage-story-module- name) ()
         ,@(when extends `((,(symb 'stage-story-module- extends))))
         ,@(when stylesheets `((load-stylesheets ',(localize-stylesheets base stylesheets))))
         ,@(when scripts `((load-scripts ',(localize-scripts base (format nil "/~(~A~)/" mname) scripts))))
         ,@(when directories `((load-directories ',(localize-directories base directories))))
         ,@(when imports `((load-imports ',(localize-imports base imports production-import-fix))))
         ,@(when dispatches `((load-module-dispatches ',dispatches)))
         ,@(when files `((load-files ',(localize-files base files))))
         ,@init
         (values)))))

(defun localize-stylesheets (base stylesheets)
  (iter (for css in stylesheets)
    (collect
        (list
         (format nil "~A~A" base css)
         (format nil "/~A" (ensure-css-extension css))))))

(defun localize-directories (base directories)
  (iter (for dir in directories)
    (let ((from (if (consp dir) (first dir) dir))
          (to (if (consp dir) (second dir) dir)))
      (collect
          (list
           (format nil "~A~A/" base from)
           (format nil "/~A/" to))))))

(defun localize-files (base files)
  (iter (for file in files)
    (let* ((from (if (consp file) (first file) file))
           (to (if (consp file) (second file) file))
           (path (format nil "~A~A" base from)))
      (collect
          (list
           path
           (format nil "/~A" to)
           (second (multiple-value-list (magic (pathname path)))))))))

(defun localize-imports (base imports &optional fix)
  (iter (for import in imports)
    (let ((file (format nil "~Aimports/~A.html" base import)))
      (collect
          (if fix
              (cons file fix)
              file)))))

(defun localize-scripts (base prefix scripts)
  (iter (for el in scripts)
    (let* ((script (if (consp el) (first el) el))
           (direct (starts-with-char script #\/))
           (fn (when (consp el) (second el))))
      (when direct (setf script (subseq script 1)))
      (unless (pathname-type script)
        (setf script (format nil "~A.js" script)))
      (collect
          (list
           (if fn
               (intern (symbol-name fn) :story-js)
               (format nil "~A~A" base script))
           (if direct
               (format nil "/~A" script)
               (format nil "~A~A" prefix script)))))))

(defun story-module-depends-on-modules (module-name)
  (iter (for name in (asdf:system-depends-on (asdf:find-system module-name)))
        (when (string-starts-with name "story-module-")
          (appending (story-module-depends-on-modules name))
          (collect (ksymb (string-upcase (subseq name (length "story-module-"))))))))

(defun all-story-modules (&key with-version)
  (iter (for system in (ql:list-local-systems))
        (when (and (string-starts-with system "story-module-")
                   (not (equal system "story-module-system")))
          (collect
              (if with-version
                  (list (subseq system 13)
                        (parse-float (asdf:component-version (asdf:find-system system))))
                  (subseq system 13))))))

(defun load-story-module (name)
  (if-let ((module (find-module (ksymb name) :errorp nil)))
    (when-let (extends (extends module))
      (load-story-module extends))
    (require (symb 'story-module- (string-upcase name)))))

(defun story-modules ()
  "Print a table of the loaded story modules."
  (let ((*print-pretty* nil))
    (iter (for (k v) in-hashtable *story-modules*)
          (format t "~A~%~@[~30Tcss  ~{~S~^, ~}~%~]~@[~30Tdir  ~{~S~^, ~}~%~]~@[~30Tjs  ~{~S~^, ~}~%~]~@[~30Tin  ~{~S~^, ~}~%~]"
                  (name v) (stylesheets v) (directories v) (scripts v) (imports v)))))

(defun stage-story-module (name &key (demo nil))
  (load-story-module name)
  (funcall (symb-in :story 'stage-story-module- (string-upcase name)))
  (when demo (story (format nil "demo-~A" name))))

(defun load-all-story-modules ()
  (mapc #'load-story-module (all-story-modules)))
