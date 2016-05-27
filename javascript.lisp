(in-package :story-js)

(defmacro define-script (name &body body)
  `(progn
     (defun ,(intern (symbol-name name) :story-js) () ,(ps* `(progn ,@body)))
     (export ',name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *slash-representative-character* #\UE000))

(defpsmacro with-id ((var id) &body body)
  `(let ((,var (id ,id)))
     (if ,var
       (progn
         ,@body))))

(defpsmacro plusp (el)
  `(> ,el 0))

(defpsmacro console (&rest rest)
  `((@ console log) ,@rest))

(defpsmacro create-element (node-type)
  `((@ document create-element) ,node-type))

(defpsmacro set-html (el html)
  `(setf (slot-value ,(if (stringp el) `(id ,el) el) 'inner-h-t-m-l) ,html))

(defpsmacro inner-html (el)
  `(slot-value ,(if (stringp el) `(id ,el) el) 'inner-h-t-m-l))

(defpsmacro remove-node (el)
  `((@ ,el parent-node remove-child) ,el))

(defun ensure-string (el)
  (if (null el) ""
    (typecase el
      (symbol (symbol-name el))
      (string el)
      (t (princ-to-string el)))))

(defun string-starts-with (string prefix &key (test #'char=))
  "Returns true if STRING starts with PREFIX."
  (let ((prefix (ensure-string prefix))
        (string (ensure-string string)))
    (let ((mismatch (mismatch prefix string :test test)))
      (or (not mismatch) (= mismatch (length prefix))))))

(defun this-swap (from to)
  (cond
    ((eql from 'this) to)
    (t
     (let ((sfrom (symbol-name from))
           (sto (symbol-name to)))
       (and (string-starts-with sfrom "THIS.")
            (intern (concatenate 'string sto "." (subseq sfrom 5))))))))

(defun subthis (this tree)
  (labels ((s (subtree)
             (or (and (symbolp subtree) (this-swap subtree this))
                 (cond ((atom subtree) subtree)
                       (t (let ((car (s (car subtree)))
                                (cdr (s (cdr subtree))))
                            (if (and (eq car (car subtree))
                                     (eq cdr (cdr subtree)))
                              subtree
                              (cons car cdr))))))))
    (s tree)))

(defpsmacro defun-trace (name args &rest body)
  (let* ((sname (ps::symbol-to-js-string name))
         (tname (ps-gensym name))
         (this (ps-gensym "this"))
         (arg-names (loop for arg in args
                          unless (eq arg '&optional)
                            collect (if (consp arg) (car arg) arg)))
         (argpairs
          (loop for arg in arg-names
                nconc (list (ps::symbol-to-js-string arg) arg))))
    `(progn
       (defun ,tname (,this ,@args)
         ,@(subthis this body))
       (defun ,name ,arg-names
         (console *trace-level* ,sname ":" ,@argpairs)
         (incf *trace-level*)
         (let ((rtn (,tname this ,@arg-names)))
           (decf *trace-level*)
           (console *trace-level* ,sname "returned" rtn)
           (return rtn))))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (when a (princ a s)))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro/ps define-story-module-parameters (module-name names &body body)
  `(progn
     ,@(loop for name in names
             collect `(defvar ,(symb '* module-name '- (if (consp name) (car name) name) '*)))
     (defun ,(symb 'setup- module-name) (&key ,@names)
       ,@(loop for el in names
               collect (let ((name (if (consp el) (car el) el)))
                         `(setf ,(symb '* module-name '- name '*) ,name)))
       ,@body)))

(defparameter *js-file*
  (concatenate
   'string
   (let ((ps:*js-string-delimiter* #\'))
     (ps*
      '(progn

        (defvar *trace-level* 0)

        (setf (@ *string prototype ends-with)
         (lambda (suffix) (return (not (== ((@ this index-of) suffix (- (@ this length) (@ suffix length))) -1)))))

        (defun when-ready (fn)
          ((@ document add-event-listener) "WebComponentsReady"
           (lambda () (funcall fn))))

        (defun visit-url (url)
          ((@ window open) url "_blank"))

        (defun id (id &optional (error t))
          (let ((hit ((@ document get-element-by-id) id)))
            (if hit
                (return hit)
                (if error (console "ERROR: id" id))))))))))

(defun js-file () *js-file*)
