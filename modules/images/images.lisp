(in-package :story)

(define-story-module images
  :init ((reset-image-processors)))

(defun exif (&rest args)
  (apply #'run-program-to-string "exif" args))

(defun parse-jpeg-exif (filename)
  (string-to-table (exif "-m" filename)))

(defun jpeg-comment (filename)
  (string-right-trim '(#\newline) (exif "-t0x9286" "-m" filename)))

(defun clean-jpeg-exif (filename)
  (iter (for id in '(#x927C ;MakerNote
                     ))
    (note "~A"
          (exif (format nil "-t0x~X" id) "--ifd=EXIF" "--remove" "-o" filename filename))))

(defun set-jpeg-comment (filename comment)
  (exif "-t0x9286" "--ifd=EXIF" "-o" filename (format nil "--set-value=\"~A\"" comment) filename))

(defun jpeg-image-size (filename)
  (if (probe-file filename)
      (values-list (mapcar #'parse-integer
                           (split-sequence #\x (third (split-sequence #\space (run-program-to-string "identify" filename))))))
      (warn "Missing ~S." filename)))

(defmacro image (&rest args)
  `(render-image stream ,@args))

(export 'image)

(defvar *image-processors*)
(defvar *valid-image-arguments*)

(defun reset-image-processors ()
  (setf *image-processors* '(default-image-processor)
        *valid-image-arguments* '(:src :alt :width :height :style)))

(defun register-image-processor (processor &rest additional-arguments)
  (if (member processor *image-processors*)
      (warn "Reregistering image processor ~S." processor)
      (setf *image-processors* (append *image-processors* (list processor))))
  (iter (for arg in additional-arguments) (pushnew arg *valid-image-arguments*)))

(defun register-image-arguments (&rest args)
  (iter (for arg in args) (pushnew arg *valid-image-arguments*)))

(defun process-image-args (args)
  (iter (for processor in *image-processors*)
    (setf args (funcall processor args)))
  args)

(defun png-image-size (filename)
  (if (probe-file filename)
      (values-list (mapcar #'parse-integer
                           (split-sequence #\x (third (split-sequence #\space (run-program-to-string "identify" filename))))))
      (warn "Missing ~S." filename)))

(defun png-comment (filename)
  (declare (ignore filename))
  (warn "PNG-COMMENT unsupported."))

(defun image-size (path)
  (if (not (probe-file path))
      (warn "Missing image ~S." path)
      (multiple-value-bind (desc mime) (magic (pathname path))
        (cond
          ((equal mime "image/png") (png-image-size path))
          ((equal mime "image/jpeg") (jpeg-image-size path))
          (t (warn "Unsupported image type ~S ~S." mime desc))))))

(defun image-comment (path)
  (if (not (probe-file path))
      (warn "Missing image ~S." path)
      (multiple-value-bind (desc mime) (magic (pathname path))
        (cond
          ((equal mime "image/png") (png-comment path))
          ((equal mime "image/jpeg") (jpeg-comment path))
          (t (warn "Unsupported image type ~S ~S." mime desc))))))

(defun default-image-processor (args)
  (let (alt src width height)
    (prog1
        (append
         (iter (for (k v) on args by 'cddr)
           (cond
             ((not (member k *valid-image-arguments*))
              (warn "Invalid image argument ~S." k))
             (t (cond
                  ((eq k :width) (setf width v))
                  ((eq k :height) (setf height v))
                  ((member k '(:src :alt))
                   (when (eq k :src) (setf src v))
                   (when (eq k :alt) (setf alt v))
                   (appending (list k v)))
                  (t (appending (list k v)))))))
         (multiple-value-bind (iw ih) (when-let (path (local-path-from-server src)) (image-size path))
           (when (or width height)
             (note "Overriding image width and height. ~A->~A ~A->~A" width iw height ih))
           `(,@(when (or width iw) `(:width ,(or width iw)))
             ,@(when (or height ih) `(:height ,(or height ih)))))
         (unless alt
           (if-let ((comment (when-let (path (local-path-from-server src)) (image-comment path))))
             `(:alt ,comment)
             (warn "Missing image :ALT ~S." src)))))))

(defun render-image (stream &rest args)
  (format stream "<img ")
  (iter (for (k v) on (process-image-args args) by 'cddr)
    (format stream "~(~A~)=~S " k v))
  (format stream ">"))

