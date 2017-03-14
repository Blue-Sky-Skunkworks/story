(in-package :story)

(defun file-mime (path)
  (run-program-to-string "file" "-i" "-b" path))

(defun render-file-viewer (query path params)
  (setf (content-type*) "text/html")
  (let ((hex (equal "hex" (cdr (assoc "view" params :test #'string=))))
        (type (pathname-type path)))
    (let ((mime (file-mime path)))
      (when-let (pos (position #\; mime))
        (setf mime (subseq mime 0 pos)))
      (if (equal type "html")
          (slurp-file path)
          (html-to-string
            (:html
              (:head
               (:title (fmt "viewing of ~S." query))
               (:link :rel "stylesheet" :type "text/css" :href "/themes/prism.css")
               (:script :type "text/javascript" :src "/prism/prism.js"))
              (:body
               (cond
                 (hex
                  (htm
                   (:pre
                    (esc (run-program-to-string "hexdump" "-C" path)))))
                 ((equal mime "text/plain")
                  (cond
                    ((member type '("css" "js") :test #'string=)
                     (htm (:pre (:code :class (format nil "language-~A" type)
                                       (esc (slurp-file path))))))
                    (t (htm (:pre (esc (slurp-file path)))))))
                 ((member mime '("image/png" "image/jpeg") :test #'string=)
                  (htm (:img :src query)))
                 (t (htm (:span (fmt "Unhandled file type ~S." mime))))))))))))
