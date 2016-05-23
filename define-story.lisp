(in-package :story)

(defvar *story* nil)
(defvar *stories* (make-hash-table :test 'equal))

(defun add-story (story)
  (when (gethash (name story) *stories*)
    (warn "Redefining story ~S." (name story)))
  (setf (gethash (name story) *stories*) story))

(defvar *story-indexes* nil)

(defun stories ()
  (setf *story-indexes*
        (iter (for (name story) in-hashtable *stories*)
              (for index from 1)
              (princ
               (let ((line (format nil "~A. ~A ~30T~A~%" index name (title story))))
                 (if (eq story *story*)
                     (white line :effect :bright)
                     line)))
              (collect (cons index name))))
  (values))

(defun story (name)
  "Select a story for display."
  (when (integerp name)
    (setf name (or (assoc-value *story-indexes* name)
                   (error "Invalid story index ~S." name))))
  (setf *story* (gethash name *stories*))
  (setup-server *story*))

(defmacro do-stories ((name story) &body body)
  `(iter (for (,name ,story) in-hashtable *stories*)
         (progn ,@body)))

(defun format-describe-line (indent els &optional (printer "~A"))
  (with-output-to-string (*standard-output*)
    (indent-text (word-wrap (format nil (format nil "~~{~A~~^, ~~}" printer) els) 100) indent :skip-first t)))

(defun describe-story (&optional (all nil))
  "Describe the story or stories."
  (macrolet ((field (name)
               `(when-let (vals (,name story))
                  (format t "    ~(~A~): ~A~%"
                          ',name (format-describe-line ,(+ 6 (length (symbol-name name))) vals)))))
    (iter (for (name story) in-hashtable *stories*)
      (for index from 1)
      (when (or all (eq story *story*))
        (format t "~A.~A ~S~%~%" index (if (eq story *story*) "*" " ") story)
        (field modules)
        (field imports)
        (field stylesheets)
        (field scripts)
        (field suffixes)
        (field prefixes)
        (field children)))))

(defun render-current-story ()
  (if *story*
      (with-output-to-string (stream)
        (render *story* stream))
      (html-to-string (:html (:head (:title "No Story"))
                             (:body "No story has been loaded.")))))

(defparameter *deploy-changed-stories* nil)

(defmacro define-story (name (&key title modules page-args package
                                stylesheets directories scripts imports suffixes prefixes
                                publish-directory cname header footer) &body body)
  `(progn
     (let* ((page (make-instance 'page :path "index.html"
                                       :renderer 'render-complete-page
                                       :body (lambda (stream page)
                                               (declare (ignorable page))
                                               (html ,@body))
                                 ,@page-args))
            (story (make-instance 'story :name ,(string-downcase name)
                                         :title ,title
                                         :home page :modules ',modules
                                         :package ,(or package :story)
                                         :directories ',directories
                                         :stylesheets ',stylesheets
                                         :imports ',imports
                                         :scripts ',scripts
                                         :suffixes ',suffixes
                                         :prefixes ',prefixes
                                         :cname ,cname
                                         :header ',header
                                         :footer ',footer
                                         :publish-directory ,(or publish-directory *publish-path*))))
       (add-child story page)
       (add-story story)
       (when *deploy-changed-stories* (story ,(string-downcase name))))))
