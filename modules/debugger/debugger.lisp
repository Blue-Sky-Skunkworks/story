(in-package :story)

(define-story-module debugger
  :imports (("debugger" debugger-interface-template))
  :sockets (("/debugger" debugger-socket-handler))
  :depends-on (:polymer :paper-input :files :packery))

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
          (".entry" :padding 10px :background "#BBB" :font-family monospace)
          (".result" :padding 10px :background "#DDD"
                     :font-family monospace :white-space pre
                     :margin 0px)
          (".result h2" :margin-top 0px)
          (".error" :padding 10px :background "#C00" :color white)
          (".description th" :text-align left))
  :content ((:div :id "workspace"
                  (input :id "repl" :on-keydown "handleKeydown" :no-label-float t)))
  :methods
  ((attached ()
             (let* ((url (+ "ws://localhost:" (@ this port) (@ this socket)))
                    (ws (new (*web-socket url))))
               (setf (@ this websocket) ws
                     (@ this commands) (create)
                     (@ ws root) this
                     (@ ws onmessage) (@ this handle-message)
                     (@ this history) (make-array)
                     (@ this aliases) (create a "alias" c "clear" h "help" d "describe"
                                              fs "fullscreen" e "evaluate"))
               ((@ this add-command) "clear" "clearRepl")
               ((@ this add-command) "fullscreen" "toggleFullscreen")
               ((@ this add-command) "describe" "describe")
               ((@ this add-command) "alias" "alias")
               ((@ this add-command) "evaluate" "evaluate")
               ((@ this $ repl focus))
               (console "debugger connected to" url)))
   (alias (&optional from to)
          (if from
              (if to
                  (progn
                    (setf (aref (@ this aliases) from) to)
                    ((@ this insert-text) (+ "Alias for \"" from "\" set.")))
                  ((@ this insert-error) "Missing TO in alias FROM TO."))
              (let ((table (story-js::create-element "table")))
                (loop for (k v) of (@ this aliases)
                      do (story-js::create-el-html* ("tr" table)
                                                    (:th k) (:td v)))
                ((@ this insert) table))))
   (alias-of (string) (or (aref (@ this aliases) string) string))
   (insert (el)
           (with-content (repl)
             (insert-before (parent-node repl) el repl)
             (flush-dom)
             ((@ repl scroll-into-view))))
   (insert-text (text &key class-name)
                ((@ this insert) (story-js::create-el-html*
                                  ("div" nil :class class-name) text)))
   (insert-error (text)
                 ((@ this insert-text) (+ "ERROR: " text) :class "error"))
   (toggle-fullscreen ()
                      (with-content (workspace)
                        (with-slots (s-position s-top s-right s-bottom s-left s-padding s-margin
                                     s-border)
                            workspace
                          (with-slots (position top right bottom left padding margin border)
                              (@ workspace style)
                            (if (@ workspace fullscreen)
                                (setf position s-position top s-top right s-right bottom
                                      s-bottom left s-left margin s-margin padding s-padding
                                      border s-border)
                                (setf s-position position position "absolute" s-top top top 0
                                      s-right right right 0 s-bottom bottom bottom 0
                                      s-left left left 0 s-margin margin margin 0
                                      s-padding padding padding 10
                                      s-border border border 0))))
                        (setf (@ workspace fullscreen) (not (@ workspace fullscreen)))))
   (handle-message (event)
                   (let ((rtn ((@ *J-s-o-n parse) (@ event data))))
                     (with-slots (class message) rtn
                       ((@ this root insert)
                        (story-js::create-el-html* ("div" nil :class class) message)))))
   (clear-repl ()
               (with-content (workspace)
                 (loop for child in (child-nodes workspace)
                       do (unless (eql (@ child id) "repl")
                            (remove-child workspace child)))))
   (add-command (command fn)
                (setf (aref (@ this commands) command) fn))
   (evaluate (&rest args)
             (let ((rtn (eval ((@ args join) " "))))
               ((@ this insert) (story-js::create-el-html ("div" nil :class "result")
                                                          ((@ this present) rtn)))))
   (describe (args)
             (let* ((obj this)
                    (el (if ((@ args starts-with) "#")
                            (or (id ((@ args substr) 1))
                                (progn
                                  ((@ this insert-error) (+ "ID \"" args "\" does not exist."))
                                  nil))
                            (eval args))))
               ((@ this insert)
                (story-js::create-el-html*
                 ("div" nil :class "result description")
                 (:h2 ((@ *object prototype to-string call) el))
                 (:table
                  ((@ (loop for key of el
                            collect
                               (htm
                                (:tr
                                 (:th key)
                                 (:td ((@ obj present) (aref el key)))))) join) ""))))))
   (handle-command (full-command)
                   ((@ this history push) full-command)
                   (with-content (repl)
                     (let* ((pos ((@ full-command index-of) " "))
                            (command (if (plusp pos) ((@ full-command substr) 0 pos) full-command))
                            (args (when (plusp pos) ((@ full-command substr) (1+ pos)))))
                       (let ((fn (aref (@ this commands) ((@ this alias-of) command))))
                         (if fn
                             (progn
                               (apply (aref this fn) (and args ((@ args split) " ")))
                               nil)
                             (+ ((@ this alias-of) command)
                                (if args (+ " " args) "")))))))
   (handle-keydown (event)
                   ;; (console (@ event key))
                   (with-content (workspace repl)
                     (cond
                       ((eql (@ event key) "Enter")
                        (let ((value (@ repl value)))
                          (when (plusp (@ value length))
                            ((@ this insert) (story-js::create-el-html*
                                              ("div" nil :class "entry") value))
                            (setf (@ repl value) "")
                            (let ((remote-command ((@ this handle-command) value)))
                              (when remote-command
                                ((@ this websocket send) remote-command))))))
                       ((eql (@ event key) "ArrowUp")
                        (with-slots (history history-index) this
                          (when (and (plusp (length history))
                                     (< history-index (length history)))
                            (setf history-index (+ history-index 1)
                                  (@ repl value) (aref history (- (length history) history-index))))))
                       ((eql (@ event key) "ArrowDown")
                        (with-slots (history history-index) this
                          (when (plusp history-index)
                            (let ((next (if (= history-index 1)
                                            ""
                                            (aref history (- (length history) history-index -1)))))
                              (setf (@ repl value) next
                                    history-index (- history-index 1))))))
                       (t (setf (@ this history-index) 0) nil))))
   (present (element)
            (let ((type (type-of element)))
              (cond
                ((eql type "number") element)
                ((eql type "boolean") element)
                ((eql type "function") type)
                ((eql type "object") element)
                ((eql type "string") element)
                (t type))))))

