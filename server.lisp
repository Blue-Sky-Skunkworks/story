(in-package :story)

(defparameter *web-port* 3300)
(defvar *web-acceptor* nil)

(defclass web-acceptor (hunchentoot:acceptor)
  ((dispatch-table :reader dispatch-table :initarg :dispatch-table)))

(defun create-exact-dispatcher (name handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
matches NAME."
  (lambda (request)
    (and (equal name (script-name request))
         handler)))

(defun format-dispatch (dispatch)
  (if (consp dispatch)
      (ecase (first dispatch)
        (:prefix (hunchentoot:create-prefix-dispatcher (second dispatch) (third dispatch)))
        (:exact (create-exact-dispatcher (second dispatch) (third dispatch)))
        (:regex (hunchentoot:create-regex-dispatcher (second dispatch) (third dispatch)))
        (:folder (hunchentoot:create-folder-dispatcher-and-handler (second dispatch) (third dispatch)))
        (:static (hunchentoot:create-static-file-dispatcher-and-handler (second dispatch) (third dispatch))))
      dispatch))

(defun start-server ()
  (when *web-acceptor*
    (warn "Server already started. Restarting")
    (hunchentoot:stop *web-acceptor*))
  (note "starting story server on port ~S" *web-port*)
  (setf *web-acceptor*
        (make-instance 'web-acceptor
                       :port *web-port*
                       :access-log-destination sb-sys:*stdout*
                       ;;(story-file (format nil "log/access-~A.log" (now)))
                       :message-log-destination sb-sys:*stdout*
                       ;;(story-file (format nil "log/message-~A.log" (now)))
                       :dispatch-table (mapcar 'format-dispatch
                                               `((:exact "/" render-current-story)
                                                 (:prefix "/css/" serve-css)
                                                 (:prefix "/" possibly-serve-directories)
                                                 (:folder "/" ,(story-file "build/"))))))
  (hunchentoot:start *web-acceptor*))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor web-acceptor) request)
  (iter (for dispatcher in (dispatch-table acceptor))
        (when-let (action (funcall dispatcher request))
          (when-let (rtn (funcall action))
            (return rtn)))
        (finally (call-next-method))))

(defparameter *css* (make-hash-table :test 'equal))

(defun load-stylesheets (&rest args)
  (iter (for (file path) on args by 'cddr)
        (setf (gethash path *css*)
              (run-program-to-string *scss-script* (list file)))))

(defparameter *scss-script* (cond
                              ((probe-file "/usr/bin/scss") "/usr/bin/scss")
                              (t (error "Missing scss."))))

(defun serve-scss-file (path)
  (let ((path (namestring path)))
    (or (and *cache-scss* (gethash path *css-files*))
        (setf (gethash path *css-files*)
              (run-program-to-string *scss-script* (list path))))))

(defun serve-css ()
  (setf (hunchentoot:content-type*) "text/css")
  (let ((url (request-uri*)))
    (or (gethash url *css*)
        (progn
          (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
          (warn "CSS miss ~S." url)))))

(defparameter *directories* (make-hash-table :test 'equal))

(defun load-directories (&rest args)



    )

(defun possibly-serve-directories ()

  )
