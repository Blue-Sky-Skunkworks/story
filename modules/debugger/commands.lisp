(in-package :story)

(defparameter *debugger-commands* '(("hilbert" . cl-ascii-art:hilbert-space-filling-curve)
                                    ("unicode" . cl-ascii-art:show-unicode-characters)
                                    ("alias" . "List and set the command aliases.")
                                    ("clear" . "Clear the workspace.")
                                    ("fullscreen" . "Toggle full screen mode.")))

(defmacro define-debugger-command (name args documentation &body body)
  (let ((fn-name (symb 'debugger-command- name))
        (cmd (string-downcase name)))
    `(progn
       (pushnew (cons ,cmd ',fn-name) *debugger-commands* :key 'car :test #'string=)
       (defun ,fn-name ,args
         ,documentation
         (let ((stream *standard-output*))
           (declare (ignorable stream))
           ,@body)))))

(define-debugger-command help (&optional command)
    "Show the debugger help."
  (html
    (:h2 "Help")
    (if command
        (let ((fn (or (assoc-value *debugger-commands* command :test 'string-equal)
                      (error "Unknown command ~S." command))))
          (htm (:h3 (esc (string-downcase command)) " "
                    (when-let ((arglist (sb-introspect:function-lambda-list fn)))
                      (esc (string-downcase (princ-to-string arglist)))))
               (:div (esc (documentation fn 'function)))))
        (htm
         (:table
          (iter (for (name . fn) in *debugger-commands*)
            (htm (:tr (:th :style "text-align:right;padding-right:10px;"
                           (esc name))
                      (:td
                       (if (stringp fn)
                           (esc fn)
                           (esc (documentation fn 'function))))))))))))

(define-debugger-command server ()
  "Describe the server."
  (let ((info (server-info)))
    (flet ((val (key) (assoc-value info key)))
      (html
        (:h2 "story server")
        (format t "port ~S~@[ address ~A~]~%" (val :port) (val :address))
        (:h3 "css")
        (iter (for (k v) in (val :css))
          (format t "  ~36A  ~@[~A~]~%" k (unless (stringp v) v)))
        (:h3 "scripts")
        (iter (for (k v) in (val :scripts)) (format t "  <a target='_blank' href='~A'>~A</a> ~@[~A~]~%"
                                                    k k
                                                    (typecase v (string nil) (t v))))
        (:h3 "directories")
        (iter (for (k v) in (val :directories)) (format t "  <a target='_blank' href='~A'>~A</a>  ~A~%" k k v))
        (:h3 "files")
        (iter (for (k type name) in (val :files)) (format t "  ~36A  ~16A ~A~%" k type name))
        (:h3 "imports")
        (iter (for (k v) in (val :imports)) (format t "  ~36A  ~A~%" v k))
        (:h3 "dispatches")
        (iter (for el in (val :dispatches)) (format t "  ~A~%" el))
        (:h3 "sockets")
        (iter (for (k v) in (val :sockets)) (format t "  ~36A  ~A~%" k v))))))

(define-debugger-command tao ()
    "The Tao Te Ching."
    (run '(fortune "tao")) (values))

