(in-package :story)

(define-story-module debugger
  :imports (("debugger" debugger-interface-template))
  :sockets (("/debugger" debugger-socket-handler))
  :depends-on (:polymer :paper-input))

(defvar *debugger* nil)

(defparameter *debugger-commands* '(("server" . server)
                                    ("hilbert" . cl-ascii-art:hilbert-space-filling-curve)
                                    ("unicode" . cl-ascii-art:show-unicode-characters)
                                    ("clear" . "Clear the REPL.")))

(defmacro define-debugger-command (name args documentation &body body)
  (let ((fn-name (symb 'debugger-command- name))
        (cmd (string-downcase name)))
    `(progn
       (pushnew (cons ,cmd ',fn-name) *debugger-commands* :key 'car :test #'string=)
       (defun ,fn-name ,args
         ,documentation
         (let ((stream *standard-output*))
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
            (htm (:tr (:th (esc name)) (:td
                                        (if (stringp fn)
                                            (esc fn)
                                            (esc (documentation fn 'function))))))))))))

(defclass debugger (websocket-resource) ())

(defmethod text-message-received ((db debugger) client message)
  (let* ((pos (position #\space message))
         (command (if pos (subseq message 0 pos) message))
         (eof (gensym))
         (args (when pos
                 (let ((*read-eval* nil))
                   (with-input-from-string (stream (subseq message pos))
                     (iter
                       (let ((arg (read stream nil eof)))
                         (if (eq arg eof)
                             (return rtn)
                             (collect arg into rtn)))))))))
    (let* ((fn (assoc-value *debugger-commands* command :test 'string=))
           (class :error)
           (rtn (with-output-to-string (*standard-output*)
                  (if fn
                      (handler-case
                          (prog1
                              (apply fn args)
                            (setf class :result))
                        (error (c)
                          (debugger-error-handler c)))
                      (debugger-command-not-found command)))))
      (send-text-message client
                         (to-json
                          (list (cons :class class)
                                (cons :message rtn)))))))

(defun debugger-error-handler (condition)
  (let ((stream *standard-output*))
   (html (esc (format t "ERROR: ~A" condition)))))

(defun debugger-command-not-found (name)
  (let ((stream *standard-output*))
    (html (:span "The command " (:b (esc name)) " is unknown."))))

(defun debugger-socket-handler (request)
  (declare (ignore request))
  (or *debugger* (setf *debugger* (make-instance 'debugger))))

(define-template debugger-interface
  :properties (("socket" string "/debugger")
               ("port" number *websocket-port*))
  :style (("#workspace" :width 500px :height 400px :border "1px solid blue"
                        :overflow-y auto :padding 10px :margin 10px)
          (".entry" :padding 10px :background "#BBB")
          (".result" :padding 10px :background "#DDD"
                     :font-family monospace :white-space pre
                     :margin 0px)
          (".result h2" :margin-top 0px)
          (".error" :padding 10px :background "#C00" :color white))
  :content ((:div :id "workspace"
                  (input :id "repl" :on-keydown "handleKeydown" :no-label-float t)))
  :methods
  ((attached ()
             (let* ((url (+ "ws://localhost:" (@ this port) (@ this socket)))
                    (ws (new (*web-socket url))))
               (setf (@ this websocket) ws
                     (@ ws root) this
                     (@ ws onmessage) (@ this handle-message))
               (console "debugger connected to" url)))
   (insert (el)
            (with-content (repl)
              (insert-before (parent-node repl) el repl)))
   (handle-message (event)
                   (let ((rtn ((@ *J-s-o-n parse) (@ event data))))
                     (with-slots (class message) rtn
                       ((@ this root insert)
                        (story-js::create-el-html* ("div" nil :class class) message))
                       ((@ this root $ repl scroll-into-view)))))
   (clear-repl ()
               (with-content (workspace)
                 (loop for child in (child-nodes workspace)
                       do (unless (eql (@ child id) "repl")
                            (remove-child workspace child)))))
   (handle-command (full-command)
                   (let* ((pos ((@ full-command index-of) " "))
                          (command (if (< 0 pos) ((@ full-command substr) 0 pos) full-command)))
                     (cond
                       ((or (eql command "clear") (eql command "c")) ((@ this clear-repl)) t)
                       (t nil))))
   (handle-keydown (event)
      (with-content (workspace repl)
        (when (= (@ event key-code) 13)
          (let ((value (@ repl value)))
            (when (< 0 (@ value length))
              ((@ this insert) (story-js::create-el-html* ("div" nil :class "entry") value))
              (setf (@ repl value) "")
              (unless ((@ this handle-command) value)
                ((@ this websocket send) value)))))))))

