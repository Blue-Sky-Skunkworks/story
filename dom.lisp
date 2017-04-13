(in-package :story-js)

(defun resolve-multiple-props (properties)
  (let (rtn)
    (loop for (k v) in properties
          do (if (consp k)
                 (loop for x in k
                       do (push (list x v) rtn))
                 (push (list k v) rtn)))
    (nreverse rtn)))

(defpsmacro dom (args &rest children)
  (destructuring-bind (node-type &optional class-name properties inner-html) (ensure-list args)
    (let ((el (gensym))
          (ch (gensym)))
      `(let ((,el ((@ document create-element) ,node-type)))
         ,@(when class-name `((aand ,class-name (add-class ,el it))))
         ,@(when inner-html `((setf (getprop ,el 'inner-h-t-m-l) ,inner-html)))
         ,@(when properties
             (loop for (k v) in (resolve-multiple-props properties)
                   collect
                      (if (story:starts-with-p (string-downcase k) "on-")
                          `((@ that listen) ,el ,(subseq (string-downcase k) 3) ,v)
                          `(setf (aref ,el ',k) ,v))))
         ,@(when children
             (loop for child in children
                   collect
                   (if (stringp child)
                       `((@ ,el append-child) (text ,child))
                       `(dom-append ,el ,child))))
         ,el))))

