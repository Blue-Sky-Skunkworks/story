(in-package :story)

(defparameter *publish-path* (story-file "build/"))

(defun write-to-file (path text)
  (with-output-to-file (stream path :if-does-not-exist :create :if-exists :supersede)
    (write-string text stream)))

(defun rsync (from to)
  (run `(rsync -a --no-l -L ,from ,to)))

(defmacro with-cd-excursion (path &body body)
  (let ((pwd (gensym)))
    `(let ((,pwd (sb-posix:getcwd)))
       (unwind-protect
            (progn
              (sb-posix:chdir ,path)
              ,@body)
         (sb-posix:chdir ,pwd)))))

(defun push-story (&optional (story *story*))
  (note "Pushing ~S." story)
  (let ((*repository* (namestring (truename (publish-directory story)))))
    (with-cd-excursion *repository*
      (note "adding")
      (git `("add" "-A" "."))
      (note "committing")
      (format t "~{;;   ~A~^~%~}" (git `("commit" "-a" "-m" ,(f "\"story push from '~A' ~A\"" (hostname) (now)))))
      (note "pushing")
      (format t "~{;;   ~A~^~%~}" (git `("push" "origin"))))))

(defun publish (&key (clear nil) (push nil) (story *story*))
  (let ((publish-path (publish-directory story)))
    (note "Publishing to ~S." publish-path)
    (flet ((path (name)
             (let ((rtn (concatenate 'string publish-path (subseq name 1))))
               (ensure-directories-exist rtn)
               rtn)))
      (setup-server story)
      (when clear
        (cond
          ((y-or-n-p "Really recursively delete ~S?" publish-path)
           (iter (for file in (directory-files publish-path))
             (note "removing ~S" file)
             (delete-file file))
           (iter (for dir in (subdirectories publish-path))
             (cond
               ((equal (last1 (pathname-directory dir)) ".git")
                (note "skipping ~S" dir))
               (t
                (note "removing ~S" dir)
                (delete-directory-tree dir :validate t)))))
          (t
           (warn "Not publishing.")
           (return-from publish))))
      (ensure-directories-exist publish-path)
      (format t "~%css:~%")
      (iter (for (k v) in-hashtable *css*)
        (format t "  ~A~%" k)
        (write-to-file (path k) v))
      (format t "~%scripts:~%")
      (iter (for (k v) in-hashtable *scripts*)
        (format t "  ~A~%" k)
        (write-to-file (path k)
                       (typecase v
                         (string v)
                         (pathname (slurp-file v))
                         (t (funcall v)))))
      (when *production*
        (format t "~%imports:~%  /all.html~%")
        (write-to-file (path "/all.html") *all-imports*))
      (when (and *production* (cname story))
        (format t "~%cname:~%  /CNAME~%")
        (write-to-file (path "/CNAME") (cname story)))
      (format t "~%directories:~%")
      (iter (for (k v) in-hashtable *directories*)
        (format t "  ~A~%" k)
        (rsync v (path k)))
      (format t "~%files:~%")
      (iter (for (k (name . type)) in-hashtable *files*)
        (format t "  ~A~%" k)
        (rsync name (path k)))
       (format t "~%pages:~%")
      (iter (for page in (children story))
        (let ((base (f "/~A" (slot-value page 'path))))
          (format t "  ~A~%" base)
          (write-to-file (path base)
                         (with-output-to-string (stream)
                           (funcall (renderer page) page stream)))))))
  ;; (format t "~%dispatches:~%")
  ;; (iter (for el in *module-dispatches*) (format t "  ~A~%" el))
  (when push (push-story story)))

(defparameter *build-server-port* (+ *web-port* 1))
(defvar *build-server* nil)

(defun serve-build (&key (port *serve-build-port*) (root (story-file "build/")))
  (when *build-server*
    (warn "Build server already started. Restarting")
    (stop *build-server*))
  (note "starting story serve-build-server on port ~S with sockets on ~S" *web-port* *websocket-port*)
  (setf *build-server*
        (make-instance 'acceptor
                       :port port
                       :document-root root
                       :access-log-destination sb-sys:*stdout*
                       :message-log-destination sb-sys:*stdout*))
  (start *build-server*))
