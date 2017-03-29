(in-package :story)

(define-story-module debugger
  :imports (("debugger" debugger-interface-template))
  :sockets (("/debugger" debugger-socket-handler))
  :depends-on (:polymer :paper-input))

(defvar *debugger* nil)

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
                          (progn
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
  :style (("#workspace" :background white
                        :border "2px solid #007" :overflow-y auto :padding 10px :margin 10px)
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
                     (@ this commands) (create)
                     (@ ws root) this
                     (@ ws onmessage) (@ this handle-message))
               ((@ this add-command) "clear" "clearRepl")
               ((@ this add-command) "fullscreen" "toggleFullscreen")
               ((@ this $ repl focus))
               (console "debugger connected to" url)))
   (insert (el)
            (with-content (repl)
              (insert-before (parent-node repl) el repl)))
   (toggle-fullscreen ()
                      (with-content (workspace)
                        (with-slots (s-position s-top s-right s-bottom s-left) workspace
                          (with-slots (position top right bottom left) (@ workspace style)
                            (if (@ workspace fullscreen)
                                (setf position s-position top s-top right s-right bottom s-bottom left s-left)
                                (setf s-position position position "absolute" s-top top top 0 s-right right right 0 s-bottom bottom bottom 0 s-left left left 0))))
                        (setf (@ workspace fullscreen) (not (@ workspace fullscreen)))))
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
   (add-command (command fn)
                (setf (aref (@ this commands) command) fn))
   (handle-command (full-command)
                   (with-content (repl)
                     (let* ((pos ((@ full-command index-of) " "))
                            (command (if (< 0 pos) ((@ full-command substr) 0 pos) full-command)))
                       (let ((fn (aref (@ this commands) command)))
                         (when fn
                           (funcall (aref this fn))
                           (setf (@ repl command-handled) t))))))
   (handle-keydown (event)
      (with-content (workspace repl)
        (when (= (@ event key-code) 13)
          (let ((value (@ repl value)))
            (when (< 0 (@ value length))
              ((@ this insert) (story-js::create-el-html* ("div" nil :class "entry") value))
              (setf (@ repl value) ""
                    (@ repl command-handled) nil)
              ((@ this handle-command) value)
              (unless (@ repl command-handled)
                ((@ this websocket send) value)))))))))

