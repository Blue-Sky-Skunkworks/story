(in-package :story-css)

(defmacro style (args &body body)
  `(with-html-output-to-string (stream nil)
     (:style ,@args
       (terpri stream)
       (str (css ,@body))))))


