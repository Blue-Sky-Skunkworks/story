(in-package :story)

(defparameter *web-port* 3300)
(defvar *web-acceptor* nil)

(defclass web-acceptor (hunchentoot:acceptor) ())

(defun create-exact-dispatcher (name handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
matches NAME."
  (lambda (request)
    (and (equal name (script-name request))
         handler)))

(defun format-dispatch (dispatch)
  (if (consp dispatch)
      (ecase (first dispatch)
        (:prefix (hunchentoot:create-prefix-dispatcher (second dispatch) (third dispatch)))
        (:exact (create-exact-dispatcher (second dispatch) (third dispatch)))
        (:regex (hunchentoot:create-regex-dispatcher (second dispatch) (third dispatch)))
        (:folder (hunchentoot:create-folder-dispatcher-and-handler (second dispatch) (third dispatch)))
        (:static (hunchentoot:create-static-file-dispatcher-and-handler (second dispatch) (third dispatch))))
      dispatch))

(defun start-server ()
  (when *web-acceptor*
    (warn "Server already started. Restarting")
    (hunchentoot:stop *web-acceptor*))
  (note "starting story server on port ~S" *web-port*)
  (reset-server)
  (setf *web-acceptor*
        (make-instance 'web-acceptor
                       :port *web-port*
                       :access-log-destination sb-sys:*stdout*
                       :message-log-destination sb-sys:*stdout*))
  (hunchentoot:start *web-acceptor*))

(defvar *imports*)

(defun load-imports (files)
  (setf *imports* (append *imports* files)))

(defvar *all-imports*)

(defun merge-pathnames-.. (to base)
  (let ((raw (namestring (merge-pathnames to base))))
    (multiple-value-bind (ms me rs re) (scan "/[^/]+?/\.\./" raw)
      (declare (ignore rs re))
      (cond
        (ms (format nil "~A/~A" (subseq raw 0 ms) (subseq raw me)))
        (t raw)))))

(defvar *current-imports* nil)

(defparameter *debug-importing* nil)

(defun collect-imports (file stream)
  "Removes comments and flattents HTML <link rel='import' ...> tags."
  (when *debug-importing* (format stream "<!-- Importing ~A -->~%" file))
  (when *current-imports* (setf (gethash (pathname-name file) *current-imports*) t))
  (let ((index 0)
        (text (slurp-file file)))
    ;; Note that the following regex has only been fleshed out enough
    ;; to work with polymer. This is not guaranteed to work with all
    ;; html. For that a true HTML parse and re-emit may be needed.
    (do-scans (ms me rs re (create-scanner "(<!--[^'].*?-->)|(<link rel=\"import\" href=\"(.+?)\">)" :single-line-mode t) text)
      (princ (subseq text index ms) stream)
      (setf index me)
      (unless (aref rs 0)
        (let ((importing (subseq text (aref rs 2) (aref re 2))))
          (setf index me)
          (when *current-imports*
            (unless (gethash (pathname-name importing) *current-imports*)
              (collect-imports (merge-pathnames-.. importing file) stream))))))
    (princ (subseq text index) stream))
  (when *debug-importing* (format stream "<!-- Done importing ~A -->~%" file))
  (values))

(defun collect-all-imports (files)
  (let ((*current-imports* (make-hash-table :test 'equal)))
    (with-output-to-string (stream)
      (iter (for el in files)
            (let ((file (if (consp el) (car el) el))
                  (fix (and (consp el) (cdr el))))
              (unless (gethash (pathname-name file) *current-imports*)
                (if fix
                    (funcall fix (with-output-to-string (fix-stream) (collect-imports file fix-stream)) stream)
                    (collect-imports file stream))))))))

(defvar *css*)

(defparameter *scss-script* (cond
                              ((probe-file "/usr/bin/scss") "/usr/bin/scss")
                              (t (error "Missing scss."))))

(defun load-stylesheets (mapping)
  (iter (for (file path) in mapping)
    (let ((text (run-program-to-string *scss-script* (list file))))
          (setf (gethash path *css*) text))))

(defvar *scripts*)

(defun load-scripts (mapping)
  (iter (for (file path) in mapping)
    (setf (gethash path *scripts*)
          (typecase file
            (string (pathname file))
            (t file)))))

(defun minimize-script (text)
  (let* ((len (length text))
         (rtn
          (handler-case
              (cl-uglify-js:ast-gen-code
               (cl-uglify-js:ast-mangle
                (cl-uglify-js:ast-squeeze
                 (parse-js:parse-js text)))
               :beautify nil)
            (error (c) (warn "Error minimilizing ~A." c) nil)))
         (lrtn (and rtn (length rtn))))
    (cond
      ((or (null rtn) (> lrtn len))
       (warn "Minimilization failed.")
       text)
      (t
       (note "Minimized script from ~D to ~D (~D%)." len lrtn (round (* (/ lrtn len) 100)))
       rtn))))

(defun remove-leading-/ (string)
  (if (char= (char string 0) #\/)
      (subseq string 1)
      string))

(defun prepare-css-for-production (prefix text)
  "Removes comments and fixes urls."
  (with-output-to-string (stream)
    (let ((index 0))
      (do-scans (ms me rs re (create-scanner "\/\\*.*?\\*\/|url\\(['\"]?(.+?)['\"]?\\)" :single-line-mode t) text)
        (princ (subseq text index ms) stream)
        (when (aref rs 0)
          (format stream "url('~A~A')" prefix (remove-leading-/ (subseq text (aref rs 0) (aref re 0)))))
        (setf index me))
      (princ (subseq text index) stream))))

(defun collect-stylesheets-and-scripts (story)
  (when-let (scripts (scripts story))
    (setf (gethash "/js.js" *scripts*) 'story-js:js-file)
    (setf (gethash "/js-all.min.js" *scripts*)
          (minimize-script
           (apply #'concatenate 'string
                  (iter (for script in (scripts story))
                    (let ((val (gethash (format nil "/~A" script) *scripts*)))
                      (collect (typecase val
                                 (pathname (slurp-file val))
                                 (string val)
                                 (null (warn "Missing script ~S." script))
                                 (t (funcall val))))))))))
  (when-let (stylesheets (stylesheets story))
    (setf (gethash "/css-all.css" *css*)
          (apply #'concatenate 'string
                 (iter (for stylesheet in stylesheets)
                   (collect (prepare-css-for-production
                             (directory-namestring stylesheet)
                             (or (gethash (format nil "/~A" (ensure-css-extension stylesheet)) *css*)
                                 (warn "Missing stylesheet ~S." stylesheet))))))))
  (when *imports*
    (setf *all-imports*
          (collect-all-imports (remove-duplicates *imports* :test 'equalp :from-end t))))
  (values))

(defvar *directories*)

(defun load-directories (mapping)
  (iter (for (dir prefix) in mapping)
    (when-let (current (gethash prefix *directories*))
      (unless (equal current dir)
        (warn "Resetting directory ~S from ~S to ~S" prefix current dir)))
    (setf (gethash prefix *directories*) dir)))

(defvar *module-dispatches*)

(defun load-module-dispatches (dispatches)
  (setf *module-dispatches* (append *module-dispatches* (mapcar 'format-dispatch dispatches))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor web-acceptor) request)
  (iter (for dispatcher in *module-dispatches*)
        (when-let (action (funcall dispatcher *request*))
          (when-let (rtn (funcall action))
            (return-from acceptor-dispatch-request rtn))))
  (let ((request-path (script-name*)) served)
    (cond
      ((string= request-path "/")
       (setf (content-type*) "text/html")
       (render-current-story))
      ((string= request-path "/all.html")
       (setf (content-type*) "text/html")
       (return-from acceptor-dispatch-request *all-imports*))
      (t
       (if-let (css (gethash request-path *css*))
         (progn
           (setf (content-type*) "text/css")
           (return-from acceptor-dispatch-request css))
         (iter (for (path file) in-hashtable *scripts*)
               (let ((mismatch (mismatch request-path path :test #'char=)))
                 (when (null mismatch)
                   (return-from acceptor-dispatch-request
                     (cond
                       ((stringp file)
                        (setf (content-type*) "text/javascript")
                        file)
                       ((pathnamep file)
                        (handle-static-file file))
                       (t
                        (setf (content-type*) "text/javascript")
                        (funcall file))))))))
       (iter (for (prefix dir) in-hashtable *directories*)
             (let ((mismatch (mismatch request-path prefix :test #'char=)))
               (when (or (null mismatch) (>= mismatch (length prefix)))
                 (handle-static-file (concatenate 'string dir (subseq request-path (length prefix))))))
             (finally (setf (return-code *reply*) +http-not-found+)
                      (abort-request-handler)))))))

(defun server ()
  "Describe the server."
  (let ((server *web-acceptor*))
    (print-heading "story server")
    (format t "port ~S~@[ address ~A~]~%" (acceptor-port server) (acceptor-address server))
    (format t "~%css:~%")
    (iter (for (k v) in-hashtable *css*) (format t "  ~A~%" k))
    (format t "~%scripts:~%")
    (iter (for (k v) in-hashtable *scripts*) (format t "  ~36A  ~S~%" k (typecase v (string :all) (t v))))
    (format t "~%directories:~%")
    (iter (for (k v) in-hashtable *directories*) (format t "  ~36A  ~A~%" k v))
    (format t "~%dispatches:~%")
    (iter (for el in *module-dispatches*) (format t "  ~A~%" el))))

(defun reset-server ()
  (setf *css* (make-hash-table :test 'equal)
        *directories* (make-hash-table :test 'equal)
        *scripts* (make-hash-table :test 'equal)
        *imports* nil
        *all-imports* ""
        *module-dispatches* nil))
