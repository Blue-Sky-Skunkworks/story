(in-package :story)

(define-story-module scratchpad
  :dispatches ((:prefix "/scratchpad/pad.html" scratchpad-renderer))
  :imports (("scratchpad" scratch-pad-template))
  :sockets (("/scratchpad" scratchpad-handler))
  :depends-on (:files :polymer))

(defvar *scratchpad* nil)

(defun scratchpad-renderer () (scratchpad-text *scratchpad*))

(defclass scratchpad (websocket-resource)
  ((text :initarg :text :initform "" :reader scratchpad-text)))

(defmethod text-message-received ((sp scratchpad) client message)
  (setf (slot-value sp 'text) message)
  (send-text-message client "reload"))

(defun scratchpad-handler (request)
  (declare (ignore request))
  (or *scratchpad* (setf *scratchpad* (make-instance 'scratchpad))))

(define-template scratch-pad
  :style ((":host" :display block :padding 20px :margin 10px)
          ("textarea" :margin 10px)
          ("iframe" :margin 10px))
  :properties (("name" string "pad")
               ("path" string "/scratchpad/")
               ("socket" string "/scratchpad")
               ("port" number 12345))
  :content ((:textarea :id "editor" :style "width:400px;height:300px;")
            (:iframe :id "frame" :style "width:400px;height:300px;" :src "{{path}}{{name}}.html"))
  :methods (attached
            (lambda ()
              (setf (@ this websocket)
                    (new (*web-socket (+ "ws://localhost:" (@ this port) (@ this socket)))))
              (let ((parent this))
                (with-slots (editor frame) (@ this $)
                  (setf (@ editor onkeyup)
                        (lambda ()
                          (when (@ parent save-timer) (clear-timeout (@ parent save-timer)))
                          (setf (@ parent save-timer)
                                (set-timeout (lambda () ((@ parent save))) 1000))
                          (set-html (@ frame content-document first-child) (@ editor value))))
                  (request (+ (@ this path) (@ this name) ".html")
                           (lambda (val) (setf (@ editor value) (@ val response))))))
              (console "scratchpad initialized"))

            save
            (lambda ()
              (let ((text (@ this $ editor value)))
                ((@ this websocket send) text)))))
