(in-package :story)

(define-story-module files
  :stylesheets (("files.css" files-css))
  :scripts (("files.js" files))
  :depends-on (:iron-request :images))

(defun create-image-thumbnail (filename)
  (run/ss `(pipe (convert ,filename -thumbnail 200 -) (base64))))

(defun create-file-listing (directory)
  (iter (for file in (directory-files directory))
    (unless (string= (pathname-name file) ".file-listing")
      (multiple-value-bind (description mime) (magic file)
        (collect (nconc (list (cons :name (pathname-name file))
                              (cons :type (pathname-type file))
                              (cons :mime mime)
                              (cons :description description)
                              (cons :thumbnail (create-image-thumbnail file)))
                        (additional-file-information (ksymb (string-upcase mime)) file)))))))

(defgeneric additional-file-information (type file)
  (:method (type file) (warn "Unhandled file type ~S." type))
  (:method ((type (eql :image/png)) file)
    (multiple-value-bind (w h) (png-image-size file)
      (list (cons :width w) (cons :height h))))
  (:method ((type (eql :image/jpeg)) file)
    (multiple-value-bind (w h) (jpeg-image-size file)
      (list (cons :width w) (cons :height h)))))

(defun save-file-listing (directory)
  (let ((filename (format nil "~A.file-listing" directory)))
    (with-output-to-file (stream filename
                                 :if-does-not-exist :create :if-exists :overwrite)
      (json:encode-json (create-file-listing directory) stream)
      (note "Wrote ~S." filename))))

(in-package :story-js)

(define-script files
  (defun fetch-file-listing (url callback)
    (request (+ "/" url "/.file-listing")
             (lambda (val) (funcall callback (eval (+ "(" (@ val response) ")"))))))

  (defun create-headings (parent)
    (set-html* (create-element "tr" parent)
               (:th "thumbnail") (:th "name") (:th "type") (:th "width") (:th "height")
               (:th "description")))

  (defun create-row (parent data)
    (let ((tr (create-element "tr" parent)))
      (set-html* tr
                 (:td (when (@ data thumbnail)
                        (ps-html
                         ((:img :src (+ "data:" (@ data mime) ";base64,"
                                        (@ data thumbnail)))))))
                 ((:td :nowrap t) (@ data name))
                 (:td (@ data mime))
                 (:td (@ data width))
                 (:td (@ data height))
                 (:td (@ data description)))))

  (defvar *create-headings-fn* (lambda (parent) (create-headings parent)))
  (defvar *create-row-fn* (lambda (parent row) (create-row parent row)))

  (defun render-file-listing (container url &key (parent-type "table"))
    (let* ((div (id container))
           (parent (create-element parent-type div)))
      (funcall *create-headings-fn* parent)
      (fetch-file-listing
       url
       (lambda (rows)
         (setf (@ div rows) rows)
         (loop for row in rows
               do (funcall *create-row-fn* parent row)))))))

(in-package :story-css)

(defun files-css ()
  (css
   '()))
