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
   (iter (for name in modules)
     (let ((module (find-module name)))
       (when-let (extends (extends module))
         (unioning (modules-and-parents (list extends))))
       (when-let (depends-on (depends-on module))
         (unioning (modules-and-parents (ensure-list depends-on)))))
     (collect name))
   :from-end t))

(defmacro when-module (name &body body)
  `(when (member ,name *story-modules*) ,@body))

(defmacro define-story-module (name &key init script-init stylesheets directories scripts
                                      imports production-import-fix depends-on
                                      extends dispatches suffixes prefixes files)
  (let* ((kname (ksymb (string-upcase name)))
         (mname (or extends name))
         (base (f "~A~(~A~)/" (story-modules-file) mname)))
    `(progn
       (setf (gethash ,kname *story-modules*)
             (make-instance 'module :name ,kname :stylesheets ',stylesheets
                                    :directories ',directories :scripts ',scripts
                                    :imports ',imports :production-import-fix ',production-import-fix
                                    :extends ,extends :dispatches ',dispatches
                                    :suffixes ',suffixes :prefixes ',prefixes :files ',files
                                    :script-init ',script-init :depends-on ',depends-on))
       (defun ,(symb 'stage-story-module- name) ()
         ,@(when extends `((,(symb 'stage-story-module- extends))))
         ,@(when depends-on (iter (for el in (ensure-list depends-on)) (collect `(,(symb 'stage-story-module- el)))))
         ,@(when stylesheets `((load-stylesheets ',(localize-stylesheets base stylesheets))))
         ,@(when scripts `((load-scripts ',(localize-scripts base (f "/~(~A~)/" mname) scripts))))
         ,@(when directories `((load-directories ',(localize-directories base directories))))
         ,@(when imports `((load-imports ',(localize-imports base (f "/~(~A~)/" mname) imports production-import-fix))))
         ,@(when dispatches `((load-dispatches ',dispatches)))
         ,@(when files `((load-files ',(localize-files base files))))
         ,@init
         (values)))))

(defun ensure-css-extension (path)
  (let ((type (pathname-type path)))
    (cond
      ((not (equalp type "css"))
       (f "~A~@[~A~]css" (subseq path 0 (- (length path) (length type)))
               (when (null type) ".")))
      (t path))))

(defun localize-stylesheets (base stylesheets)
  (iter (for el in stylesheets)
    (destructuring-bind (css &optional fn) (ensure-list el)
      (collect
          (list
           (if fn
               (intern (symbol-name fn) :story-css)
               (f "~A~A" base css))
           (f "/~A" (ensure-css-extension css)))))))

(defun localize-directories (base directories)
  (iter (for dir in directories)
    (let ((from (if (consp dir) (first dir) dir))
          (to (if (consp dir) (second dir) dir)))
      (collect
          (list
           (if (string-starts-with from "/")
               from
               (f "~A~A/" base from))
           (f "/~A/" to))))))

(defun localize-files (base files)
  (iter (for file in files)
    (let* ((from (if (consp file) (first file) file))
           (fn (when (consp file) (second file)))
           (path (or fn (f "~A~A" base from)))
           (mime (if fn
                     (or (third file) (mimes:mime-lookup from))
                     (second (multiple-value-list (magic (pathname path)))))))
      (if (or fn (probe-file path))
          (collect
              (list
               path
               (f "/~A" from)
               mime))
          (warn "Missing file ~S." path)))))

(defun localize-imports (base prefix imports &optional fix)
  (iter (for el in imports)
    (destructuring-bind (name &optional fn) (ensure-list el)
      (collect
          (nconc
           (list
            (if fn
                fn ;; (intern (symbol-name fn) :story-css)
                (f "~Aimports/~A.html" base name))
            (f "~A~A.html" prefix name))
           (when fix (list fix)))))))

(defun localize-scripts (base prefix scripts)
  (iter (for el in scripts)
    (let* ((script (if (consp el) (first el) el))
           (direct (starts-with-char script #\/))
           (fn (when (consp el) (second el))))
      (when direct (setf script (subseq script 1)))
      (unless (pathname-type script)
        (setf script (f "~A.js" script)))
      (collect
          (list
           (if fn
               (intern (symbol-name fn) :story-js)
               (f "~A~A" base script))
           (if direct
               (f "/~A" script)
               (f "~A~A" prefix script)))))))

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
  (when demo (story (f "demo-~A" name))))

(defun load-all-story-modules ()
  (mapc #'load-story-module (all-story-modules)))
