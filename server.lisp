(in-package :story)

(defparameter *web-port* 3300)
(defvar *server* nil)

(defclass server (hunchentoot:acceptor)
  ((css :reader css)))

(defun create-exact-dispatcher (name handler)
  "Creates a request dispatch function which will dispatch to the
function denoted by HANDLER if the file name of the current request
matches NAME."
  (lambda (request)
    (and (equal name (script-name request))
         handler)))

(defun string-or-val (el)
  (etypecase el
    (string el)
    (symbol (symbol-value el))))

(defun format-dispatch (dispatch)
  (if (consp dispatch)
      (ecase (first dispatch)
        (:prefix (hunchentoot:create-prefix-dispatcher (second dispatch) (third dispatch)))
        (:exact (create-exact-dispatcher (second dispatch) (third dispatch)))
        (:regex (hunchentoot:create-regex-dispatcher (second dispatch) (third dispatch)))
        (:folder (hunchentoot:create-folder-dispatcher-and-handler (second dispatch) (string-or-val (third dispatch))))
        (:static (hunchentoot:create-static-file-dispatcher-and-handler (second dispatch) (third dispatch) (fourth dispatch))))
      dispatch))

(defun start-server ()
  (when *server*
    (warn "Server already started. Restarting")
    (hunchentoot:stop *server*))
  (note "starting story server on port ~S" *web-port*)
  (reset-server)
  (setf *server*
        (make-instance 'server
                       :port *web-port*
                       :access-log-destination sb-sys:*stdout*
                       :message-log-destination sb-sys:*stdout*))
  (hunchentoot:start *server*))

(defvar *imports*)
(defvar *import-fns*)

(defun load-imports (files)
  (iter (for (local path) in files)
    (when (symbolp local)
      (setf (gethash path *import-fns*) local)))
  (setf *imports* (append *imports* files)))

(defvar *all-imports*)

(defun merge-pathnames-.. (to base)
  (let ((raw (namestring (merge-pathnames to base))))
    (multiple-value-bind (ms me rs re) (scan "/[^/]+?/\.\./" raw)
      (declare (ignore rs re))
      (cond
        (ms (f "~A/~A" (subseq raw 0 ms) (subseq raw me)))
        (t raw)))))

(defvar *current-imports* nil)

(defparameter *debug-importing* nil)

(defun collect-imports (file stream)
  "Removes comments and flattents HTML <link rel='import' ...> tags."
  (when *debug-importing* (format stream "<!-- Importing ~A -->~%" file))
  (when *current-imports* (setf (gethash
                                 (etypecase file
                                   (symbol file)
                                   (string (pathname-name file)))
                                 *current-imports*) t))
  (let ((index 0)
        (text (etypecase file
                (symbol (funcall file))
                (string (slurp-file file)))))
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
      (iter (for (file local fix) in files)
        (unless (gethash (etypecase file
                           (symbol file)
                           (string (pathname-name file)))
                         *current-imports*)
          (if fix
              (funcall fix (with-output-to-string (fix-stream) (collect-imports file fix-stream)) stream)
              (collect-imports file stream)))))))

(defvar *css*)

(defparameter *scss-script* (cond
                              ((probe-file "/usr/bin/scss") "/usr/bin/scss")
                              ((probe-file "/usr/local/bin/scss") "/usr/local/bin/scss")
                              (t (error "Missing scss."))))

(defun load-stylesheets (mapping)
  (iter (for (file path) in mapping)
    (setf (gethash path *css*)
          (typecase file
            (string (run/s (list *scss-script* file)))
            (t file)))))

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
    (let ((all (minimize-script
                (apply #'concatenate 'string
                       (iter (for script in scripts)
                         (let ((val (gethash (f "/~A" script) *scripts*)))
                           (collect (typecase val
                                      (pathname (slurp-file val))
                                      (string val)
                                      (null (warn "Missing script ~S." script))
                                      (t (funcall val))))))))))
      (setf *scripts* (make-hash-table :test 'equal)
            (gethash "/js-all.min.js" *scripts*) all)))
  (when-let (stylesheets (stylesheets story))
    (let ((all (apply #'concatenate 'string
                      (iter (for stylesheet in stylesheets)
                        (collect (prepare-css-for-production
                                  (directory-namestring stylesheet)
                                  (or (gethash (f "/~A" (ensure-css-extension stylesheet)) *css*)
                                      (warn "Missing stylesheet ~S." stylesheet))))))))
      (setf *css* (make-hash-table :test 'equal)
            (gethash "/css-all.css" *css*) all)))
  (when *imports*
    (setf *all-imports* (collect-all-imports (remove-duplicates *imports* :test 'equalp :from-end t))
          *imports* '("/all.html")))
  (values))

(defvar *directories*)

(defun load-directories (mapping)
  (iter (for (dir prefix) in mapping)
    (when-let (current (gethash prefix *directories*))
      (unless (equal current dir)
        (warn "Resetting directory ~S from ~S to ~S" prefix current dir)))
    (setf (gethash prefix *directories*) dir)))

(defvar *files*)

(defun load-files (mapping)
  (iter (for (dir prefix content-type) in mapping)
    (when-let (current (gethash prefix *files*))
      (unless (equal current dir)
        (warn "Resetting file ~S from ~S to ~S" prefix current dir)))
    (setf (gethash prefix *files*) (cons dir content-type))))

(defvar *module-dispatches*)

(defun load-dispatches (dispatches)
  (setf *module-dispatches* (append *module-dispatches* (mapcar 'format-dispatch dispatches))))

(defvar *directory-listing-fn* nil)
(defvar *file-argument-handler* nil)

;;; serve-all
(defmethod hunchentoot:acceptor-dispatch-request ((acceptor server) request)
  (iter (for dispatcher in *module-dispatches*)
    (when-let (action (funcall dispatcher *request*))
      (when-let (rtn (funcall action))
        (return-from acceptor-dispatch-request rtn))))
  (let ((query (script-name*)))
    (acond

      ((string= query "/")              ; root
       (setf (content-type*) "text/html")
       (render-current-story))

      ((and *production* (string= query "/all.html")) ; all html imports
       (setf (content-type*) "text/html")
       *all-imports*)

      ((gethash query *import-fns*)     ; import functions
       (setf (content-type*) "text/html")
       (funcall it))

      ((gethash query *css*)            ; stylesheets
       (setf (content-type*) "text/css")
       (etypecase it
         (symbol (funcall it))
         (string it)))

      ((iter (for (path file) in-hashtable *scripts*) ; javascript
             (when (null (mismatch query path :test #'char=)) (return file)))
       (etypecase it
         (string (setf (content-type*) "text/javascript") it)
         (pathname (handle-static-file it))
         (symbol (setf (content-type*) "text/javascript") (funcall it))))

      ((iter (for (prefix dir) in-hashtable *directories*) ; directories
         (let ((mismatch (mismatch query prefix :test #'char=)))
           (when (or (null mismatch) (>= mismatch (length prefix)))
             (return (list prefix dir)))))
       (destructuring-bind (prefix dir) it
         (let ((path (concatenate 'string dir (subseq query (length prefix))))
               (params (get-parameters*)))
           (cond
             ((and *directory-listing-fn* (char= #\/ (aref path (1- (length path)))))
              (funcall *directory-listing-fn* query path))
             ((and *file-argument-handler* (assoc "view" params :test #'string=))
              (funcall *file-argument-handler* query path params))
             (t (handle-static-file path))))))

      ((iter (for (prefix (file . content-type)) in-hashtable *files*) ; files
             (when (string= prefix query) (return (list file content-type))))
       (destructuring-bind (file content-type) it
           (cond
             ((symbolp file)
              (setf (content-type*) content-type)
              (funcall file))
             (t (handle-static-file file content-type)))))

      (t                                ; not found
       (setf (return-code *reply*) +http-not-found+)
       (abort-request-handler)))))

(defun server-info ()
  (let ((server *server*))
    (list
     (cons :port (acceptor-port server))
     (cons :address (acceptor-address server))
     (cons :css
           (iter (for (k v) in-hashtable *css*)
             (collect (list k v))))
     (cons :scripts
           (iter (for (k v) in-hashtable *scripts*)
             (collect (list k v))))
     (cons :directories
           (iter (for (k v) in-hashtable *directories*)
             (collect (list k v))))
     (cons :files
           (iter (for (k (name . type)) in-hashtable *files*)
             (collect (list k type name))))
     (cons :imports
           (iter (for el in (remove-duplicates *imports* :test 'equal :from-end t))
             (collect el)))
     (cons :dispatches
           (iter (for el in *module-dispatches*)
             (collect el))))))

(defun server ()
  "Describe the server."
  (print-heading "story server")
  (let ((info (server-info)))
    (flet ((val (key) (assoc-value info key)))
      (format t "port ~S~@[ address ~A~]~%" (val :port) (val :address))
      (format t "~%css:~%")
      (iter (for (k v) in (val :css)) (format t "  ~36A  ~@[~A~]~%" k (unless (stringp v) v)))
      (format t "~%scripts:~%")
      (iter (for (k v) in (val :scripts)) (format t "  ~36A  ~@[~A~]~%" k (typecase v (string nil) (t v))))
      (format t "~%directories:~%")
      (iter (for (k v) in (val :directories)) (format t "  ~36A  ~A~%" k v))
      (format t "~%files:~%")
      (iter (for (k type name) in (val :files)) (format t "  ~36A  ~16A ~A~%" k type name))
      (format t "~%imports:~%")
      (iter (for (k v) in (val :imports)) (format t "  ~36A  ~A~%" v k))
      (format t "~%dispatches:~%")
      (iter (for el in (val :dispatches)) (format t "  ~A~%" el)))))

(defun reset-server ()
  (setf *css* (make-hash-table :test 'equal)
        *directories* (make-hash-table :test 'equal)
        *files* (make-hash-table :test 'equal)
        *scripts* (make-hash-table :test 'equal)
        *import-fns* (make-hash-table :test 'equal)
        *imports* nil
        *all-imports* ""
        *module-dispatches* nil)
  ;(reset-image-processors)
  )


(defun local-path-from-server (path)
  (if (scan "https?://" path)
      (note "Skipping url ~S." path)
      (progn
       (when-let (pos (position #\/ path))
         (when-let (hit (gethash (f "/~A/" (subseq path 0 pos)) *directories*))
           (return-from local-path-from-server (concatenate 'string hit (subseq path (1+ pos))))))
       (warn "Missing local path ~S." path))))
