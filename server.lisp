(in-package :story)

(defparameter *web-port* 3300)
(defvar *web-acceptor* nil)

(defclass web-acceptor (hunchentoot:acceptor)
  ((dispatches :reader dispatches :initarg :dispatches)
   (dispatch-table :reader dispatch-table)))

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

(defmethod initialize-instance :after ((acceptor web-acceptor) &key)
  (when (dispatches acceptor)
    (setf (slot-value acceptor 'dispatch-table) (mapcar 'format-dispatch (dispatches acceptor)))))

(defun start-server ()
  (when *web-acceptor*
    (warn "Server already started. Restarting")
    (hunchentoot:stop *web-acceptor*))
  (note "starting story server on port ~S" *web-port*)
  (reset-server)
  (setf *web-acceptor*
        (make-instance 'web-acceptor
                       :port *web-port*
                       :access-log-destination sb-sys:*stdout*
                       ;;(story-file (format nil "log/access-~A.log" (now)))
                       :message-log-destination sb-sys:*stdout*
                       ;;(story-file (format nil "log/message-~A.log" (now)))
                       :dispatches `((:exact "/" render-current-story)
                                     (:prefix "/css/" serve-css)
                                     (:prefix "/" serve-all))))
  (hunchentoot:start *web-acceptor*))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor web-acceptor) request)
  (iter (for dispatcher in (dispatch-table acceptor))
        (when-let (action (funcall dispatcher request))
          (when-let (rtn (funcall action))
            (return rtn)))
        (finally (call-next-method))))

(defvar *css*)

(defparameter *scss-script* (cond
                              ((probe-file "/usr/bin/scss") "/usr/bin/scss")
                              (t (error "Missing scss."))))

(defun load-stylesheets (&rest args)
  (iter (for (file path) on args by 'cddr)
        (let ((text (run-program-to-string *scss-script* (list file))))
          (setf (gethash path *css*) text))))

(defun serve-css ()
  (setf (hunchentoot:content-type*) "text/css")
  (let ((url (request-uri*)))
    (or (gethash url *css*)
        (progn
          (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
          (warn "CSS miss ~S." url)))))

(defvar *scripts*)

(defun load-scripts (args)
  (iter (for (file path) on args by 'cddr)
        (setf (gethash path *scripts*)
              (typecase file
                (string (pathname file))
                (t file)))))

(defun collect-stylesheets-and-scripts (story)
  (when (scripts story)
    (setf (gethash "/js/js.js" *scripts*) 'story-js:js-file)
    (setf (gethash "/js/js-all.js" *scripts*)
          (apply #'concatenate 'string
                 (iter (for script in (scripts story))
                       (let ((val (gethash (format nil "/js/~A" script) *scripts*)))
                         (collect (typecase val
                                    (pathname (slurp-file val))
                                    (string val)
                                    (t (funcall val)))))))))
  (when (stylesheets story)
    (setf (gethash "/css/css-all.css" *css*)
          (apply #'concatenate 'string
                 (iter (for stylesheet in (stylesheets story))
                       (collect (gethash (format nil "/css/~A" stylesheet) *css*)))))))


(defvar *directories*)

(defun load-directories (&rest args)
  (iter (for (dir prefix) on args by 'cddr)
        (when-let (current (gethash prefix *directories*))
          (unless (equal current dir)
            (warn "Resetting directory ~S from ~S to ~S" prefix current dir)))
        (setf (gethash prefix *directories*) dir)))

(defun serve-all ()
  (let ((request-path (script-name*)) served)
    (iter (for (path file) in-hashtable *scripts*)
          (let ((mismatch (mismatch request-path path :test #'char=)))
            (when (null mismatch)
              (setf served t)
              (cond
                ((stringp file)
                 (setf (hunchentoot:content-type*) "text/javascript")
                 (return-from serve-all file))
                ((pathnamep file) (handle-static-file file))
                (t
                 (setf (hunchentoot:content-type*) "text/javascript")
                 (return-from serve-all (funcall file)))))))
    (unless served
      (iter (for (prefix dir) in-hashtable *directories*)
            (let ((mismatch (mismatch request-path prefix :test #'char=)))
              (and (or (null mismatch) (>= mismatch (length prefix)))
                   (handle-static-file (merge-pathnames dir request-path))))
            (finally (setf (return-code *reply*) +http-not-found+)
                     (abort-request-handler))))))

(defun server ()
  "Describe the server."
  (let ((server *web-acceptor*))
    (print-heading "story sever")
    (format t "port ~S~@[ address ~A~]~%~%" (acceptor-port server) (acceptor-address server))
    (iter (for dispatch in (dispatches server))
          (format t "  ~6A  ~10A  ~A~%" (first dispatch) (second dispatch) (third dispatch)))
    (format t "~%css:~%")
    (iter (for (k v) in-hashtable *css*) (format t "  ~A~%" k))
    (format t "~%scripts:~%")
    (iter (for (k v) in-hashtable *scripts*) (format t "  ~36A  ~S~%" k (typecase v
                                                                          (string :all)
                                                                          (t v))))
    (format t "~%directories:~%")
    (iter (for (k v) in-hashtable *directories*) (format t "  ~36A  ~A~%" k v))))

(defun reset-server ()
  (setf *css* (make-hash-table :test 'equal)
        *directories* (make-hash-table :test 'equal)
        *scripts* (make-hash-table :test 'equal)))
