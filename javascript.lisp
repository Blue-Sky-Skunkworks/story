(in-package :story-js)

(import '(story::html-to-string))

(defmacro+ps anaphoric (op test &body body)
  `(let ((it ,test)) (,op it ,@body)))

(defmacro+ps aand (first &rest rest) `(anaphoric and ,first ,@rest))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (when a (princ a s)))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defmacro define-script (name &body body)
  `(progn
     (defun ,(intern (symbol-name name) :story-js) () ,(ps* `(progn ,@body)))))

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

(defpsmacro create-element (node-type &optional parent class-name)
  (let ((el (gensym)))
    `(let ((,el ((@ document create-element) ,node-type)))
       ,@(when parent `(((@ ,parent append-child) ,el)))
       ,@(when class-name `((add-class ,el ,class-name)))
       ,el)))

(defpsmacro set-html (el html)
  `(let ((myel ,(if (stringp el) `(id ,el) el)))
     (setf (getprop myel 'inner-h-t-m-l) ,html)
     myel))

(defpsmacro set-html* (el &rest html)
  (let ((el (if (stringp el) `(id ,el) el)))
    `(let ((node ,el))
       (setf (getprop node 'inner-h-t-m-l) (parenscript:who-ps-html ,@html))
       node)))

(defpsmacro create-el-html* ((node-type &optional parent &key class) &body html)
  `(set-html* (create-element ,node-type ,parent ,class) ,@html))

(defpsmacro create-html (parent &body html)
  `(let ((el (create-element "div")))
     (set-html* el ,@html)
     ((@ ,parent append-child) (@ el first-child))))

(defpsmacro inner-html (el)
  `(slot-value ,(if (stringp el) `(id ,el) el) 'inner-h-t-m-l))

(defpsmacro htm (&rest html-forms)
  `(who-ps-html ,@html-forms))

(defpsmacro remove-node (el)
  `((@ ,el parent-node remove-child) ,el))

(defpsmacro get-prototype-of (object)
  `((@ *object get-prototype-of) ,object))

(defpsmacro when-let ((variable initial-form) &body body)
  `(let ((,variable ,initial-form))
     (when ,variable
       ,@body)))

(defun ensure-string (el)
  (if (null el) ""
    (typecase el
      (symbol (symbol-name el))
      (string el)
      (t (princ-to-string el)))))

(defun this-swap (from to)
  (cond
    ((eql from 'this) to)
    (t
     (let ((sfrom (symbol-name from))
           (sto (symbol-name to)))
       (and (story::starts-with-p sfrom "THIS.")
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
         (tname (ps-gensym (princ-to-string name)))
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

(defun maximize-string-length (els)
  (let ((max 0))
    (loop for string in (mapcar #'princ-to-string els)
          do (let ((len (length string))) (when (> len max) (setf max len))))
    max))

(defpsmacro define-story-module-parameters (module-name names &body body)
  `(progn
     ,@(loop for name in names
             collect `(defvar ,(symb '* module-name '- (if (consp name) (car name) name) '*)))
     (defun ,(symb 'setup- module-name) (&key ,@names)
       ,@(loop for el in names
               collect (let ((name (if (consp el) (car el) el)))
                         `(setf ,(symb '* module-name '- name '*) ,name)))
       ,@body)
     (defun ,(symb 'describe-module- module-name) ()
       (console ,(format nil "module: ~(~A~)" module-name))
       ,@(let ((max (maximize-string-length names)))
           (loop for el in names
                 collect (let ((name (if (consp el) (car el) el)))
                           `(console ,(format nil (format nil "  ~~(~~~AA~~)" (+ max 2)) name) ,(symb '* module-name '- name '*))))))))

(defparameter *js-file*
  (concatenate
   'string

   (let ((ps:*js-string-delimiter* #\'))
     (ps*
      '(progn

        (defvar *trace-level* 0)

        (defun create-el (tag props &optional parent)
          (let ((el ((@ document create-element) tag)))
            (for-in (n props) (setf (getprop el n) (getprop props n)))
            (when parent ((@ parent append-child) el))
            el))

        (defun text (arg) ((@ document create-text-node) arg))

        (defun mkstr (&rest args)
          ((@ (loop for arg in args
                    when arg collect arg)
              join) ""))

        (defun mapcar (fun &rest arrs)
          (let ((result-array (make-array)))
            (if (= 1 (length arrs))
                (dolist (element (aref arrs 0))
                  ((@ result-array push) (fun element)))
                (dotimes (i (length (aref arrs 0)))
                  (let ((args-array (mapcar (lambda (a) (aref a i)) arrs)))
                    ((@ result-array push) ((@ fun apply) fun args-array)))))
            result-array))

        (defun type-of (o)
          (let ((type (typeof o)))
            (cond
              ((and (eql type "object") o (instanceof o *array)) "array")
                                        ; ((eql type "object") nil)
              (t type))))

        (defun arrayp (o) (eql (type-of o) "array"))
        (defun stringp (o) (eql (type-of o) "string"))
        (defun functionp (o) (eql (type-of o) "function"))
        (defun objectp (o) (eql (type-of o) "object"))

        (defun function-from-string (name)
          (let ((el (eval name)))
            (and (functionp el) el)))

        (defun add-class (el name)
          (setf (@ el class-name) (+ (@ el class-name) (if (@ el class-name) " " "") name)))

        (defun has-class (el name)
          ((@ el class-list contains) name))

        (unless (@ *string prototype ends-with)
          (setf (@ *string prototype ends-with)
                (lambda (suffix) (not (equal ((@ this index-of) suffix
                                              (- (@ this length) (@ suffix length)))
                                             -1)))))

        (unless (@ *string prototype starts-with)
          (setf (@ *string prototype starts-with)
                (lambda (prefix &optional (position 0))
                  (eql ((@ this substr) position (@ prefix length)) prefix))))

        (defun when-ready (fn)
          ((@ document add-event-listener) "WebComponentsReady"
           (lambda () (funcall fn))))

        (defun visit-url (url)
          ((@ window open) url "_blank"))

        (defun id (id &optional (error t))
          (or ((@ document get-element-by-id) id)
              (when error ((@ console error) "ERROR: id" id))))

        (defun dom-append (el ch)
          (when ch
            (if (arrayp ch)
                (loop for c in ch
                      when c
                        do ((@ el append-child)
                            (if (and c (@ c node-type)) c (text c))))
                ((@ el append-child)
                 (if (and ch (@ ch node-type)) ch (text ch))))))

        (defun hide (el)
          (setf (@ el style display) "none"))

        (defun hidden (el) (eql (@ el style display) "none"))
        (defun visible (el) (not (eql (@ el style display) "none")))

        (defun show (el &key (val "block"))
          (setf (@ el style display) val))

        (defun event-el (event type)
          (loop for el in (@ event path)
                when (eql type (@ el local-name))
                  do (return el)))

        (defun previous-visible-sibling (el)
          (loop for prev = (@ el previous-sibling) then (@ prev previous-sibling)
                while prev
                when (visible prev) do (return prev)))

        (defun next-visible-sibling (el)
          (loop for next = (@ el next-sibling) then (@ next next-sibling)
                while next
                when (visible next) do (return next)))

        )))))

(defun js-file () *js-file*)
