(in-package :story)

(defun pdf-info (file)
  (remove-if #L(member (cdr %) '("no" "none") :test #'equal)
             (mapcar #L(split-on-first-occurance
                        % #\: #L(string-trim '(#\space) %))
                     (run/lines `("pdfinfo" ,file)))))

(defmethod additional-file-information ((type (eql :application/pdf)) file)
  (list (cons :info (pdf-info file))
        (cons :thumbnail (create-image-thumbnail (f "~A[0]" file)))))

(defun extract-pdf-page (file page &optional (destination (story-file "modules/files/tmp-pdf-pages/")))
  (ensure-directories-exist destination)
  (let ((name (f "~A~A-p~A" destination (pathname-name file) page)))
    (run `(pdfimages "-png" "-f" ,page "-l" ,page ,file ,name))
    (f "~A-000.png" name)))

(defun render-pdf-info (stream file)
  (html
    (:h1 (emoji (file-icon "application/pdf") t) (esc (pathname-name file)))
    (:img :src (f "data:image/png;base64,~A" (create-image-thumbnail (f "~A[0]" file) 400)))
    (:img :src (f "data:image/png;base64,~A" (create-image-thumbnail (f "~A[1]" file) 400)))
    (:img :src (f "data:image/png;base64,~A" (create-image-thumbnail (f "~A[2]" file) 400)))
    (:table
     (iter (for (k . v) in (pdf-info file))
       (htm (:tr (:th (esc k)) (:td (esc v))))))))
