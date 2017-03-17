(in-package :story)

(define-story-module fabric
  :scripts ("fabric.min.js" ("fab.js" fabric-js)))

(in-package :story-js)

(defpsmacro rect (&rest args) `(new ((@ fabric *rect) (create ,@args))))
(defpsmacro text (text &rest args) `(new ((@ fabric *text) ,text (create ,@args))))
(defpsmacro on (el event-name &body body)
  `((@ ,el on) ,event-name (lambda (event) (let ((e (@ event e))) ,@body))))
(defpsmacro add (&rest els) `(progn ,@(loop for el in els collect `((@ *fabric-canvas* add) ,el))))
(defpsmacro left (el) `(@ ,el left))
(defpsmacro top (el) `(@ ,el top))
(defpsmacro width (el) `(@ ,el width))
(defpsmacro height (el) `(@ ,el height))
(defpsmacro bring-to-front (el) `((@ *fabric-canvas* bring-to-front) ,el))
(defpsmacro with-image ((var url) &body body)
  `((@ fabric *image from-u-r-l) ,url (lambda (,var) ,@body)))


(defpsmacro with-fabric (var &body body)
  `(let ((*fabric-canvas* ,var))
     ,@body))

(define-script fabric-js
  (defvar *fabric-canvas*)

  (defun initialize-fabric (canvas-id)
    (let* ((canvas (id canvas-id))
           (fabric-canvas (new ((@ fabric *canvas) canvas-id (create 'fire-middle-click t)))))
      (setf (@ canvas fabric-canvas) fabric-canvas)
      fabric-canvas)))







