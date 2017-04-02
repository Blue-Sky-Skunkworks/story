(in-package :story)

(define-story-module debugger
  :imports (("debugger" debugger-interface-template))
  :sockets (("/debugger" debugger-socket-handler))
  :depends-on (:polymer :paper-input :files :packery :prism))

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
          ("div.divider" :height 5px :background "#BBB")
          ("div.error" :padding 10px :background "#C00" :color white)
          ;; (".description table" :border-collapse collapse)
          ;; (".description tr.owned")
          (".description h3" :white-space normal)
          (".description th" :text-align left :font-family monospace :padding-right 10px)
          ("code.language-js" :font-family monospace)
          ("span.desc" :color blue :cursor pointer)
          ("span.error" :color red))
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
                                              fs "fullscreen" e "evaluate" rv "reverse-video"))
               ((@ this add-command) "clear" "clearRepl")
               ((@ this add-command) "fullscreen" "toggleFullscreen")
               ((@ this add-command) "dom" "insertDom")
               ((@ this add-command) "describe" "describe")
               ((@ this add-command) "alias" "alias")
               ((@ this add-command) "evaluate" "evaluate")
               ((@ this add-command) "reverse-video" "reverseVideo")
               ((@ this $ repl focus))
               (console "debugger connected to" url)))
   (alias (&optional from to)
          (if from
              (if to
                  (progn
                    (setf (aref (@ this aliases) from) to)
                    ((@ this insert-text) (+ "Alias for \"" from "\" set.")))
                  ((@ this insert-error) "Missing TO in alias FROM TO."))
              (let ((table (dom :table)))
                (loop for (k v) of (@ this aliases)
                      do (append-child table
                                       (dom :tr
                                            (dom :th k)
                                            (dom :td v))))
                ((@ this insert) table))))
   (alias-of (string) (or (aref (@ this aliases) string) string))
   (insert (el)
           (with-content (repl)
             (when (aand (@ repl previous-sibling) (has-class it "result"))
               (insert-before (parent-node repl) (dom (:div "divider")) repl))
             (insert-before (parent-node repl) el repl)
             (flush-dom)
             ((@ repl focus))))
   (insert-text (text &key class-name)
                ((@ this insert) (dom (:div class-name) text)))
   (insert-error (text)
                 ((@ this insert-text) (+ "ERROR: " text) :class-name "error"))
   (reverse-video ()
                  (setf (@ document body style filter)
                        (if (plusp (length (@ document body style filter))) "" "invert(100%)")))
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
                        (dom ("div" class nil message))))))
   (clear-repl ()
               ((@ console clear))
               (with-content (workspace)
                 (loop for child in (child-nodes workspace)
                       do (unless (eql (@ child id) "repl")
                            (remove-child workspace child)))))
   (add-command (command fn)
                (setf (aref (@ this commands) command) fn))
   (evaluate (&rest args)
             (console :evaluate args)
             (let ((rtn (eval ((@ args join) " "))))
               (console :return rtn)
               ((@ this insert) (dom (:div "result") ((@ this present) rtn)))))
   (prototypes-of (el)
                  (when (objectp el)
                    (loop
                      for pro = (get-prototype-of el) :then (get-prototype-of pro)
                      while pro
                      collect pro)))
   (_parse_id (arg)
              (if ((@ arg starts-with) "#")
                  (or (id ((@ arg substr) 1))
                      (progn
                        ((@ this insert-error) (+ "ID \"" arg "\" does not exist."))
                        nil))
                  (eval arg)))
   (insert-dom (arg)
               (let ((root ((@ this _parse_id) arg))
                     (obj this))
                 (flet ((recur (el indent)
                          ((@ obj insert)
                           (dom (:div nil ((style (+ "padding-left:" (* indent 20) "px"))))
                                ((@ obj present) el)))
                          (loop for child in (@ el children)
                                do (recur child (1+ indent)))))
                   (recur root 0))))
   (_describe (el)
              (console :describe el)
              (when el
                (let ((obj this))
                  ((@ this insert)
                   (dom (:div "result description")
                        (dom :h2 ((@ obj present) el))
                        (when (objectp el)
                          (dom :h3 (loop for pro in ((@ obj prototypes-of) el)
                                         collect ((@ obj present) pro)
                                         collect (text " "))))
                        (dom :table
                             (loop for key of el
                                   collect
                                   (dom (:tr (when ((@ el has-own-property) key) "owned"))
                                        (dom :th key)
                                        (dom :td ((@ obj present)
                                                  (try
                                                   (aref el key)
                                                   (:catch (error) error)))))))
                        (cond
                          ((ignore-errors (eql (@ el node-name) "STYLE"))
                           (let ((pre
                                   (dom :pre
                                        (dom (:code "language-css") (@ el inner-text)))))
                             ((@ *prism highlight-element) (@ pre first-child))
                             pre))
                          ((functionp el)
                           (let ((pre
                                   (dom :pre
                                        (dom (:code "language-js") ((@ el to-string))))))
                             ((@ *prism highlight-element) (@ pre first-child))
                             pre))))))))
   (describe (arg)
             ((@ this _describe)
              (if ((@ arg starts-with) "#")
                  (or (id ((@ arg substr) 1))
                      (progn
                        ((@ this insert-error) (+ "ID \"" arg "\" does not exist."))
                        nil))
                  (eval arg))))
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
                   (with-content (workspace repl)
                     (with-slots (history history-index) this
                       (cond
                         ((eql (@ event key) "Enter")
                          (let ((value (@ repl value)))
                            (when (plusp (@ value length))
                              ((@ this insert) (dom ("div" "entry") value))
                              (setf (@ repl value) "")
                              (let ((remote-command ((@ this handle-command) value)))
                                (when remote-command
                                  ((@ this websocket send) remote-command))))))
                         ((eql (@ event key) "ArrowUp")
                          (when (and (plusp (length history))
                                     (< history-index (length history)))
                            (setf history-index (+ history-index 1)
                                  (@ repl value) (aref history (- (length history) history-index)))))
                         ((eql (@ event key) "ArrowDown")
                          (when (plusp history-index)
                            (let ((next (if (= history-index 1)
                                            ""
                                            (aref history (- (length history) history-index -1)))))
                              (setf (@ repl value) next
                                    history-index (- history-index 1)))))
                         (t (setf (@ this history-index) 0) nil)))))
   (handle-present-tap (event) ((@ this _describe) (@ event target presenting)))
   (handle-present-keys (event)
                        (when (eql (@ event key) "Enter")
                          ((@ this _describe) (@ event target presenting))))
   (present (element)
            (let ((type (type-of element)))
              (cond
                ((eql type "number") element)
                ((eql type "boolean") element)
                ((eql type "function") type
                 (dom (:span "desc" ((on-tap "handlePresentTap") (on-keypress "handlePresentKeys")
                                     (presenting element) (tab-index 1)))
                      (+ "[fn " (if (@ element name) (+ " " (@ element name)) "") "]")))
                ((eql type "object")
                 (if (eql element nil)
                     "null"
                     (let* ((type (or (try ((@ element node-name to-lower-case)) (:catch (error) nil))
                                      ((@ ((@ *object prototype to-string call) element) slice) 8 -1))))
                       (dom (:span (+ "desc" (if (eql type "Error") " error" ""))
                                   ((on-tap "handlePresentTap") (on-keypress "handlePresentKeys")
                                    (presenting element) (tab-index 1)))
                            (+ "[" (case type
                                     ("Error" (@ element name))
                                     (otherwise type))
                               "]")))))
                ((eql type "string") (+ "\"" element "\""))
                ((eql type "array") (+ "[" ((@ element to-string)) "]"))
                ((eql type "undefined") type)
                (t (+ "UNHANDLED: " type)))))))

