(in-package :story)

;;; element

(defclass element ()
  ((parent :reader parent :initform nil :initarg :parent)
   (children :reader children :initform nil :initarg :children)))

(defmethod add-child ((parent element) (child element))
  (cond
    ((member child (children parent))
     (warn "~S is already a child of ~S." child parent))
    (t
     (when (parent child)
       (warn "Replacing parent of ~S ~S with ~S." child (parent child) parent))
     (setf (slot-value child 'parent) parent
           (slot-value parent 'children) (append (children parent) (list child))))))

;; module

(defclass module (element)
  ((name :reader name :initarg :name)
   (stylesheets :reader stylesheets :initarg :stylesheets)
   (directories :reader directories :initarg :directories)
   (scripts :reader scripts :initarg :scripts)
   (imports :reader imports :initarg :imports)
   (production-import-fix :reader production-import-fix :initarg :production-import-fix)
   (extends :reader extends :initarg :extends)
   (dispatches :reader dispatches :initarg :dispatches)
   (suffixes :reader suffixes :initarg :suffixes)
   (prefixes :reader prefixes :initarg :prefixes)))

;;; story

(defclass story (module)
  ((package :reader story-package :initarg :package)
   (publish-directory :reader publish-directory :initarg :publish-directory)
   (title :reader title :initarg :title)
   (home :reader home :initarg :home)
   (modules :reader modules :initarg :modules :initform nil)
   (cname :reader cname :initarg :cname)))

(defmethod print-object ((story story) stream)
  (print-unreadable-object (story stream :type t)
    (format stream "~A" (name story))))

(defmacro define-story-accessor (slot)
  `(defmethod ,slot ((story story))
     (iter (for name in (modules-and-parents (modules story)))
       (let ((module (find-module name)))
         (when-let (els (,slot module))
           (appending els into rtn)))
       (finally (return (append rtn (slot-value story ',slot)))))))

(define-story-accessor directories)
(define-story-accessor stylesheets)

(defmethod scripts ((story story))
  (let (include-js)
    (iter
      (for name in (modules-and-parents (modules story)))
      (let ((module (find-module name)))
        (when-let (els (scripts module))
          (setf include-js t)
          (appending
           (iter (for el in els)
             (let* ((fn (and (consp el) (second el)))
                    (script (if fn (car el) el))
                    (path
                      (if (starts-with-char script #\/)
                          (subseq script 1)
                          (format nil "~(~A~)/~A" (or (extends module) name)
                                  (if (null (pathname-type script))
                                      (format nil "~A.js" script)
                                      script)))))
               (collect path))) into rtn)))
      (finally (return
                 (append
                  (if include-js (cons "js.js" rtn) rtn)
                  (mapcar (lambda (el) (if (consp el) (car el) el))
                          (slot-value story 'scripts))))))))

(defmethod imports ((story story))
  (iter
    (for name in (modules-and-parents (modules story)))
    (let ((module (find-module name)))
      (when-let (els (imports module))
        (appending
         (iter (for el in els)
           (collect (format nil "~(~A~)/~A.html" (or (extends module) name) el)))
         into rtn)))
    (finally (return (append rtn (mapcar (lambda (el) (format nil "imports/~A.html" el)) (slot-value story 'imports)))))))

(defmethod suffixes ((story story))
  (iter
    (for name in (modules-and-parents (modules story)))
    (let ((module (find-module name)))
      (when-let (els (suffixes module))
        (appending
         (iter (for el in els)
           (collect (format nil "modules/~(~A~)/~A" (or (extends module) name) el))) into rtn)))
    (finally (append rtn (slot-value story 'suffixes)))))

(defmethod prefixes ((story story))
  (iter
    (for name in (modules-and-parents (modules story)))
    (let ((module (find-module name)))
      (when-let (els (prefixes module))
        (appending
         (iter (for el in els)
           (collect (format nil "modules/~(~A~)/~A" (or (extends module) name) el))) into rtn)))
    (finally (return (append rtn (slot-value story 'prefixes))))))

(defun setup-server (story)
  (reset-server)
  (let ((base (asdf-base-path (story-package story))))
    (when (modules story)
      (mapc #'stage-story-module (modules story))
      (load-directories (localize-directories base (slot-value story 'directories)))
      (load-imports (localize-imports base (slot-value story 'imports)))
      (load-scripts (localize-scripts base "/" (slot-value story' scripts)))
      (load-scripts '((story-js:js-file "/js.js")))
      (when *production* (collect-stylesheets-and-scripts story)))))

;;; page

(defclass page (element)
  ((path :reader path :initarg :path)
   (title :reader title :initform nil :initarg :title)
   (renderer :reader renderer :initarg :renderer)
   (body-class :reader body-class :initarg :body-class :initform nil)
   (body :reader body :initarg :body)))

(defmethod print-object ((page page) stream)
  (print-unreadable-object (page stream :type t)
    (format stream "~A" (path page))))
