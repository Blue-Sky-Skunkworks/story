(in-package :story)

(define-story-module debugger
  :imports (("debugger" debugger-interface-template))
  :sockets (("/debugger" debugger-socket-handler))
  :depends-on (:polymer :paper-input))

(defvar *debugger* nil)

(defparameter *debugger-commands* '(("server" . server)
                                    ("hilbert" . cl-ascii-art:hilbert-space-filling-curve)
                                    ("unicode" . cl-ascii-art:show-unicode-characters)))

(defmacro define-debugger-command (name args documentation &body body)
  (let ((fn-name (symb 'debugger-command- name))
        (cmd (string-downcase name)))
    `(progn
       (pushnew (cons ,cmd ',fn-name) *debugger-commands* :key 'car :test #'string=)
       (defun ,fn-name ,args
         ,documentation
         (let ((stream *standard-output*))
           ,@body)))))

(define-debugger-command help ()
  "Show the debugger help."
  (html
    (:h2 "Help")
    (:table
     (iter (for (name . fn) in *debugger-commands*)
       (htm (:tr (:th (esc name)) (:td (esc (documentation fn 'function)))))))))

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

(ps:defpsmacro dom (el) `((@ *polymer dom) ,el))
(ps:defpsmacro parent-node (node) `(@ (dom ,node) parent-node))
(ps:defpsmacro insert-before (parent node before-node)
  `((@ (dom ,parent) insert-before) ,node ,before-node))
(ps:defpsmacro with-content (ids &body body)
  `(with-slots ,ids (@ this $) ,@body))

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
   (handle-command (command) (console command))
   (handle-keydown (event)
      (with-content (workspace repl)
        (when (= (@ event key-code) 13)
          (let ((value (@ repl value)))
            ((@ this insert) (story-js::create-el-html* ("div" nil :class "entry") value))
            (setf (@ repl value) "")
            (unless ((@ this handle-command) value)
              ((@ this websocket send) value))))))))

