(in-package :story)

(define-story-module debugger
  :imports (("debugger" debugger-interface-template))
  :sockets (("/debugger" debugger-socket-handler))
  :depends-on (:polymer :paper-input))

(defvar *debugger* nil)

(defparameter *debugger-commands* '(("help" . debugger-command-help)))

(defun debugger-command-help (&rest args)
  (format nil "Story Debugger Help"))

(defclass debugger (websocket-resource) ())

(defmethod text-message-received ((db debugger) client message)
  (let* ((pos (position #\space message))
         (command (if pos (subseq message 0 pos) message)))
    (let ((fn (assoc-value *debugger-commands* command :test 'string=)))
      (send-text-message client (if fn (funcall fn)
                                    (debugger-command-not-found command))))))

(defun debugger-command-not-found (name)
  (format nil "The command ~S is not known." name))

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
          (".entry" :padding 10px :background "#EEE")
          (".reply" :padding 10px))
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
                   ((@ this root insert)
                    (story-js::create-el-html* ("div" nil :class "reply") (@ event data))))
   (handle-keydown (event)
      (with-content (repl)
        (when (= (@ event key-code) 13)
          ((@ this websocket send) (@ repl value))
          ((@ this insert) (story-js::create-el-html* ("div" nil :class "entry") (@ repl value)))
          (setf (@ repl value) "")
          ((@ repl scroll-into-view)))))))




