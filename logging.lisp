(in-package :story)

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

(defmethod hunchentoot:acceptor-log-access ((acceptor server) &key return-code)
  (note "~A ~:[ ~@[ (~A)~]~;~:*~A~@[ (~A)~]~]~:[~;~: *~A~] ~A ~A ~16A ~A  ~A  ~A"
        (cyan "A" :effect :bright)
        (short-remote-addr)
        (header-in* :x-forwarded-for)
        (authorization)
        (with-output-to-string (stream)
          (with-color ((cond
                         ((eql return-code +http-ok+) :blue)
                         ((eql return-code +http-not-modified+) :cyan)
                         (t :red))
                       :stream stream :effect :bright)
            (format stream "~3D" return-code)))
        (short-user-agent)
        (short-content-type*)
        (or (and (content-length*) (f "~7D" (content-length*)))
            (if (eql return-code +http-not-modified+)
                "       "
                (red "none" :effect :bright)))
        (f "~36A"
           (let ((referer (referer))
                 (server-prefix (f "http://localhost:~D" *web-port*)))
             (cond
               ((null referer) "-")
               ((string-starts-with referer server-prefix) (subseq referer (length server-prefix)))
               (t referer))))
        (with-output-to-string (stream)
          (with-color ((if (member return-code '(200 304) :test 'eql) :white :red) :stream stream :effect :bright)
            (format stream "~@[~A ~]~A~@[?~A~]~@[ ~A~]"
                    (unless (equal (request-method*) :get) (request-method*))
                    (script-name*)
                    (query-string*)
                    (unless (member (server-protocol*) *known-server-protocols* :test 'string=)
                      (red (server-protocol*) :effect :bright)))))))

(defun short-content-type* ()
  (let ((mime (let ((raw (content-type*)))
                (if-let ((pos (position #\; raw)))
                  (subseq raw 0 pos)
                  raw))))
    (cond
      ((string= mime "text/html") :html)
      ((string= mime "text/css") :css)
      ((string= mime "text/javascript") :js)
      ((string= mime "application/octet-stream") :bytes)
      ((string= mime "image/png") :bytes)
      (t mime))))

(defmethod acceptor-log-message ((acceptor server) log-level format-string &rest format-arguments)
  (handler-case
      (note "~@[~A~] ~A~%"
            (and log-level
                 (with-output-to-string (stream)
                   (with-color ((case log-level
                                  (:error :red)
                                  (:warning :yellow)
                                  (:info :white)) :stream stream :effect :bright)
                     (format stream "~A" log-level))))
            (colorize-message format-string format-arguments))
    (error (e)
      (ignore-errors
        (format *trace-output* "error ~A while writing to error log, error not logged~%" e)))))

(defun colorize-message (fmt args)
  (with-output-to-string (stream)
    (let ((raw (apply #'format nil fmt args))
          (start 0))
      (do-scans (ms me rs re "((?<!\\\\)\"(.*?)(?<!\\\\)\")|({[0-9][0-9A-F]*})| ([0-9][0-9A-F]*)|( :[-A-Z0-9]+)|\\((ERROR|LAMBDA|SIGNAL)|([0-9]+:)" raw)
        ;;(note "~A ~A ~A ~A" ms me rs re)
        (format stream "~A" (subseq raw start ms))
        (setf start me)
        (with-color ((cond
                       ((aref rs 0) :green)
                       ((aref rs 2) :magenta)
                       ((aref rs 3) :cyan)
                       ((aref rs 4) :yellow)
                       ((aref rs 5) :white)
                       ((aref rs 6) :red))
                     :stream stream :effect :bright)
          (format stream "~A" (subseq raw ms me))))
      (format stream "~A" (subseq raw start)))))
