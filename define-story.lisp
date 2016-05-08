(in-package :story)

(defvar *story* nil)
(defvar *stories* (make-hash-table :test 'equal))

(defun add-story (story)
  (when (gethash (name story) *stories*)
    (warn "Redefining story ~S." (name story)))
  (setf (gethash (name story) *stories*) story
        *story* story))

(defmacro do-stories ((name story) &body body)
  `(iter (for (,name ,story) in-hashtable *stories*)
         (progn ,@body)))

(defun story (&optional (all nil))
  "Describe the story or stories."
  (iter (for (name story) in-hashtable *stories*)
        (for index from 1)
        (when (or all (eq story *story*))
          (format t "~A.~A ~S~%" index (if (eq story *story*) "*" " ") story)
          (when (modules story) (format t "    modules: ~{~A~^, ~}~%" (modules story)))
          (when (imports story) (format t "    imports: ~{~A~^, ~}~%" (imports story)))
          (when (stylesheets story) (format t "        css: ~{~S~^, ~}~%" (stylesheets story)))
          (when (scripts story) (format t "    scripts: ~{~S~^, ~}~%" (scripts story)))
          (when (suffixes story) (format t "   suffixes: ~{~S~^, ~}~%" (suffixes story)))
          (when (prefixes story) (format t "   prefixes: ~{~S~^, ~}~%" (prefixes story)))
          (iter (for child in (children story))
                (format t "      ~S~%" child)))))

(defun render-current-story ()
  (with-output-to-string (stream)
    (render *story* stream)))

(defmacro define-story (name (&key title modules) &body body)
  `(progn
     (reset-server)
     (let* ((page (make-instance 'page :path "index.html" :renderer 'render-complete-page
                                :body (lambda (stream page)
                                        (declare (ignorable page))
                                        (html ,@body))))
           (story (make-instance 'story :name ,(string-downcase name) :title ,title
                                 :home page :modules ',modules)))
      (add-child story page)
      (add-story story))))
