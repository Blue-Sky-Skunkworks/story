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


;;; story

(defclass story (element)
  ((name :reader name :initarg :name)
   (title :reader title :initarg :title :initform "Unititled Story")
   (home :reader home :initarg :home)
   (modules :reader modules :initarg :modules :initform nil)
   (stylesheets :reader stylesheets :initform nil)
   (scripts :reader scripts :initform nil)
   (imports :reader imports :initform nil)
   (suffixes :reader suffixes :initform nil)
   (prefixes :reader prefixes :initform nil)
   (production :reader production :initform *production* :initarg :production)))

(defmethod print-object ((story story) stream)
  (print-unreadable-object (story stream :type t)
    (format stream "~A" (name story))))

(defun setup-server (story)
  (when (modules story)
    (mapc #'load-story-module (modules story))
    (setf (slot-value story 'imports) (collect-module-imports (modules story))
          (slot-value story 'stylesheets) (collect-module-stylesheets (modules story))
          (slot-value story 'suffixes) (collect-module-suffixes (modules story))
          (slot-value story 'prefixes) (collect-module-prefixes (modules story)))
    (when-let ((scripts (collect-module-scripts (modules story))))
      (setf (slot-value story 'scripts) (cons "js.js" scripts)))
    (collect-stylesheets-and-scripts story)))

;;; page

(defclass page (element)
  ((path :reader path :initarg :path)
   (title :reader title :initform nil :initarg :title)
   (renderer :reader renderer :initarg :renderer)
   (body :reader body :initarg :body)))

(defmethod print-object ((page page) stream)
  (print-unreadable-object (page stream :type t)
    (format stream "~A" (path page))))
