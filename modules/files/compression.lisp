(in-package :story)

(defmethod additional-file-information ((type (eql :application/gzip)) file)
  (list (cons :info (gzip-info file))))

(defun gzip-info (path)
  (let ((info (split-sequence #\space (second (run/lines `(gzip -l ,path))) :remove-empty-subseqs t)))
    (list (cons :compressed (parse-integer (first info)))
          (cons :uncompressed (parse-integer (second info))))))

(defun render-gzip-info (stream file)
  (html
    (:h1 (emoji "compression") (esc (pathname-name file)))
    (:table
     (iter (for (k . v) in (gzip-info file))
       (htm (:tr (:th (esc (princ-to-string k))) (:td (esc (princ-to-string v)))))))))
