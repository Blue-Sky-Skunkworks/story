(in-package :story)

(defun render-file-viewer (query path params)
  (setf (content-type*) "text/html")
  (let ((hex (equal "hex" (cdr (assoc "view" params :test #'string=))))
        (type (pathname-type path)))
    (multiple-value-bind (description mime) (magic path)
      (if (equal type "html")
          (slurp-file path)
          (html-to-string
            (:html
              (:head
               (:title (fmt "viewing of ~S." query)))
              (:body
               (cond
                 (hex
                  (htm
                   (:pre
                    (esc (run-program-to-string "hexdump" "-C" path)))))
                 ((member mime '("application/octet-stream") :test #'equal)
                  (htm (:pre (esc (slurp-file path)))))
                 (t (htm (:span (fmt "Unhandled file type ~S." mime))))))))))))
