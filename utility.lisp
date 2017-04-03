(in-package :story)

(enable-sharpl-reader)

(defmacro html (&body body) `(with-html-output (stream nil) ,@body))
(defmacro html-to-string (&body body) `(with-html-output-to-string (stream nil) ,@body))

(defun story-file (&optional base)
  (namestring (asdf:system-relative-pathname :story base)))

(defmacro f (&rest args) `(format nil ,@ args))

(defmacro vertical-break (&optional (height "20px"))
  `(html (:div :style ,(format nil "height:~A;" height))))

(defvar *note-lock* (sb-thread:make-mutex))
(defparameter *inhibit-note* nil)

(defmacro with-note-lock (&body body) `(with-mutex (*note-lock*) ,@body))

(defparameter *note-start-clock* (let ((now (get-universal-time)))
                                   (format t "~&;;  Note logging started at: ~A.~%" now)
                                   now))

(defparameter *show-note-clock* nil)

(defun note (control &rest arguments)
  (unless *inhibit-note*
    (let ((*print-pretty* nil))
      (sb-thread:with-mutex (*note-lock*)
        (apply #'format t
               (format nil "~~&;; ~@[ ~A ~]~A ~A~~%"
                       (and *show-note-clock* (blue (princ-to-string (clock-face)) :effect :bright))
                       (blue (princ-to-string (- (get-universal-time) *note-start-clock*)) :effect :bright)
                       control) arguments)
        (finish-output t)))))

(defmacro bugout (&rest vars)
  "Print VARS, for debugging."
  (when-let ((len (iter (for var in vars)
                        (maximizing (length (prin1-to-string var))))))
    `(note ,(with-output-to-string (s)
                                   (write-string "BUGOUT  " s)
                                   (iter (for els on vars)
                                         (let ((var (car els)))
                                           (format s (format nil "~~~AS" (+ len 2)) var)
                                           (unless (keywordp var) (write-string " ==>  ~S" s))
                                           (when (cdr els) (format s "~%;;               ")))))
           ,@(remove-if #'keywordp vars))))

(defun ensure-trailing-slash (string &optional (slash-character #\/))
  (if (char= (aref string (1- (length string))) slash-character)
    string
    (with-output-to-string (stream)
      (write-string string stream)
      (write-char slash-character stream))))

(defun string-to-table (string)
  (mapcar #L(split-sequence #\tab %)
          (split-sequence #\newline string :remove-empty-subseqs t)))

(defun slurp-file (filename &optional external-format)
  (with-input-from-file (stream filename :external-format (or external-format :utf-8))
    (let* ((len (file-length stream))
           (seq (make-string len))
           (actual-len (read-sequence seq stream)))
      (if (< actual-len len)
        ;; KLUDGE eh, FILE-LENGTH doesn't know about utf8 so we use some duct tape
        (string-right-trim '(#\nul) seq)
        seq))))

(defmacro to-json (item)
  `(json:encode-json-to-string ,item))

(defmacro from-json (item)
  `(json:decode-json-from-string ,item))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (when a (princ a s)))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun symb-in (package &rest args)
  (values (intern (apply #'mkstr args) package)))

(defun ksymb (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

(defmacro with-assoc-values ((alist names &key (test 'eql)) &body body)
  (with-gensyms (data)
    `(let* ((,data ,alist)
            ,@(iter (for name in (ensure-list names))
                    (collect `(,(symb (string-upcase name)) (assoc-value ,data ,name :test ',test)))))
       ,@body)))

(defun print-heading (text &key (underline "-"))
  (write-string text)
  (fresh-line)
  (dotimes (i (length text)) (write-string underline))
  (fresh-line)
  (terpri))

(defmacro script (&body body)
  `(html (:script (str (ps (progn ,@body))))))

(defmacro script* (&body body)
  `(html (:script (str (ps* ,@body)))))

(defmacro with-words ((var sentence) &body body)
  (let ((raw (gensym)))
    `(iter (for ,raw in (split-sequence #\space ,sentence :remove-empty-subseqs t))
       (let ((,var (remove #\newline ,raw)))
         ,@body))))

(defun expand-home-path (path)
  (if (char= (char path 0) #\~)
      (format nil "~A~A" (uiop/os:getenv "HOME") (subseq path 1))
      path))

(defmacro vertical-break (&optional (height "20px"))
  `(html (:div :style ,(format nil "height:~A;" height))))

(defmacro comment (text &key (indent 2))
  `(html (str (format nil "<!--~%~%~A~%-->" ,(if indent
                                                 `(as-string (indent-text ,text ,indent))
                                                 text)))))

(defun last1 (list)
  (car (last list)))

(defun group (list n)
  "Group the elements of LIST into lists of N elements each."
  (when (zerop n) (error "Groups of zero are no fun."))
  (labels ((rec (list acc)
             (let ((rest (nthcdr n list)))
               (if (consp rest)
                 (rec rest (cons
                            (subseq list 0 n)
                            acc))
                 (nreverse
                  (cons list acc))))))
    (when list (rec list nil))))

(defmacro carr (el)
  (let ((var (gensym)))
    `(let ((,var ,el))
       (if (consp ,var)
           (car ,var)
           ,var))))

(defun render-tree (stream fn tree)
  (labels ((recur (el level)
             (funcall fn stream level (car el))
             (iter (for child in (cdr el))
                   (recur child (1+ level)))))
    (recur tree 0)))

(defun split-on-first-occurance (string char &optional (fn #'identity))
  (if-let ((pos (position char string)))
    (cons (funcall fn (subseq string 0 pos)) (funcall fn (subseq string (1+ pos))))
    string))

(defun starts-with-char (string char)
  (char= char (char string 0)))

(let (original-eval-region)
  (defun naked-eval-region (string)
    (if (and (plusp (length string)) (starts-with-char string #\())
      (funcall original-eval-region string)
      (let ((first-word
              (if-let (pos (position #\space string))
                (subseq string 0 pos)
                (string-right-trim '(#\newline) string))))
        (if (fboundp (find-symbol (string-upcase first-word)))
          (funcall original-eval-region (format nil "(~A)" string))
          (funcall original-eval-region string)))))
  (defun use-naked-repl (&optional (enable t))
    (if enable
      (if original-eval-region
        (format t "Already using a naked repl.~%")
        (progn
          (setf original-eval-region (symbol-function (find-symbol "EVAL-REGION" :swank))
                (symbol-function (find-symbol "EVAL-REGION" :swank)) #'naked-eval-region)
          (format t "Now using a naked repl.~%")))
      (progn
        (when original-eval-region
          (setf (symbol-function (find-symbol "EVAL-REGION" :swank)) original-eval-region
                original-eval-region nil))
        (format t "No longer using a naked repl.~%")))
    (values)))

(defmacro ql (name) `(ql:quickload ',name))

(defun starts-with-p (seq subseq)
  (let ((subseq-len (length subseq)))
    (if (<= subseq-len (length seq))
	(search subseq seq :end2 subseq-len)
	nil)))

(defun ends-with-p (seq subseq)
  (let ((seq-len (length seq))
	(subseq-len (length subseq)))
    (if (<= (length subseq) (length seq))
	(search subseq seq :from-end t :start2 (- seq-len subseq-len))
	nil)))

(in-package :json)

(defmethod encode-json ((p pathname) &optional (stream *json-output*))
  (write-json-string (namestring p) stream))



