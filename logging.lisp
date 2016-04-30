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

(defmethod hunchentoot:acceptor-log-access ((acceptor web-acceptor) &key return-code)
  (note "~A ~:[-~@[ (~A)~]~;~:*~A~@[ (~A)~]~] ~:[-~;~:*~A~] ~A ~A ~A  ~A  ~A"
        (cyan "A" :effect :bright)
        (short-remote-addr)
        (header-in* :x-forwarded-for)
        (authorization)
        (with-output-to-string (stream)
          (with-color ((if (eql return-code 200) :blue :red) :stream stream :effect :bright)
            (format stream "~3D" return-code)))
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

(defmethod acceptor-log-message ((acceptor web-acceptor) log-level format-string &rest format-arguments)
  (handler-case
      (note "~@[~A~] ~?~%"
            (and log-level
                 (with-output-to-string (stream)
                   (with-color ((case log-level
                                  (:error :red)
                                  (:warning :yellow)
                                  (:info :white)) :stream stream :effect :bright)
                     (format stream "~A" log-level))))
            format-string format-arguments)
    (error (e)
      (ignore-errors
        (format *trace-output* "error ~A while writing to error log, error not logged~%" e)))))
