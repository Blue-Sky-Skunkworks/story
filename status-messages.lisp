(in-package :story)

(defmethod acceptor-status-message ((acceptor server) http-status-code &rest args &key &allow-other-keys)
  (call-next-method))

(defmethod acceptor-status-message ((acceptor server) (http-status-code (eql +http-not-found+)) &rest args &key &allow-other-keys)
  (declare (ignore args))
  (html-to-string
   (:html
     (fmt "~%<!-- story ~A ~C ~A -->~%" (git-latest-commit) (clock-face) (format-timestring nil (now)))
     (:head (:title "404 Not Found"))
     (:body :style "padding:20px;"
      (:h1 "Not Found")
      (fmt "<i>~A</i> is not here." (hunchentoot:script-name*))))))
