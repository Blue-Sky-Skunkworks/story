(in-package :story)

(defmacro image (&rest args)
  `(htm (:img ,@(process-image-args args))))

(defvar *image-processors* '(default-image-processor))
(defvar *valid-image-arguments* '(:src :alt :width :height))

(defun register-image-processor (processor &rest additional-arguments)
  (when (member processor *image-processors*) (warn "Reregistering image processor ~S." processor))
  (pushnew processor *image-processors*)
  (iter (for arg in additional-arguments) (pushnew arg *valid-image-arguments*)))

(defun process-image-args (args)
  (iter (for processor in *image-processors*)
    (setf args (funcall processor args)))
  args)

(defun png-image-size (filename)
  (if (probe-file filename)
      (values-list (mapcar #'parse-integer (split-sequence #\x (third (split-sequence #\space (run-program-to-string "identify" (list filename)))))))
      (warn "Missing ~S." filename)))

(defvar *image-sizes* (make-hash-table :test 'equal))

(defun image-size (path)
  (if-let ((hit (gethash path *image-sizes*)))
    (values-list hit)
    (if (not (probe-file path))
        (warn "Missing image ~S." path)
        (multiple-value-bind (desc mime) (magic (pathname path))
          (cond
            ((equal mime "image/png") (png-image-size path))
            (t (warn "Unsupported image type ~S ~S." mime desc)))))))

(defun default-image-processor (args)
  (let (alt src width height)
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
               (appending (list k v)))))))
     (multiple-value-bind (iw ih) (when-let (path (local-path-from-server src)) (image-size path))
       (when (or width height)
         (warn "Overriding image width and height. ~A->~A ~A->~A" iw width ih height))
       (unless alt (warn "Missing image :ALT ~S." src))
       (list
        :width (or width iw)
        :height (or height ih))))))

