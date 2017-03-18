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

