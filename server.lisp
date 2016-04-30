(in-package :story)

(defparameter *web-port* 3300)
(defvar *web-acceptor* nil)

(defclass web-acceptor (hunchentoot:acceptor)
  ((dispatch-table :reader dispatch-table :initarg :dispatch-table)))

(defparameter *known-server-protocols* '("HTTP/1.1"))

(defun short-remote-addr ()
  (let ((addr (remote-addr*)))
    (cond
      ((string= addr "127.0.0.1") "lo")
      (t addr))))

(defun short-user-agent ()
  (let ((agent (user-agent)))
    (cond
      ((scan (ppcre:create-scanner "chrome" :case-insensitive-mode t) agent) "chrome")
      (t  addr))))

(defmethod hunchentoot:acceptor-log-access ((acceptor web-acceptor) &key return-code)
  (note "~A ~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] ~A ~A ~A  ~A  ~A"
        (cyan "A" :effect :bright)
        (short-remote-addr)
        (header-in* :x-forwarded-for)
        (authorization)
        (with-output-to-string (stream)
          (with-color ((if (eql return-code 200) :blue :red) :stream stream :effect :bright)
            (format stream "~D" return-code)))
        (short-user-agent)
        (or (and (content-length*) (format nil "~4D" (content-length*)))
            (red "none" :effect :bright))
        (format nil "~30A"
                (let ((referer (referer))
                      (server-prefix (format nil "http://localhost:~D" *web-port*)))
                  (cond
                    ((null referer) "-")
                    ((string-starts-with referer server-prefix) (subseq referer (length server-prefix)))
                    (t referer))))
        (with-output-to-string (stream)
          (with-color ((if (eql return-code 200) :white :red) :stream stream :effect :bright)
            (format stream "~A ~A~@[?~A~]~@[ ~A~]"
                    (request-method*)
                    (script-name*)
                    (query-string*)
                    (unless (member (server-protocol*) *known-server-protocols* :test 'string=)
                      (red (server-protocol*) :effect :bright)))))))

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
          (return (funcall action)))
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
