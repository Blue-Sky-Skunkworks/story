(in-package :story)

(defmacro html (&body body) `(with-html-output (stream nil) ,@body))
(defmacro html-to-string (&body body) `(with-html-output-to-string (stream nil) ,@body))

(defun asdf-base-path (name)
  (directory-namestring (asdf:component-pathname (asdf:find-system name))))

(defun story-file (&optional  base)
  (concatenate 'string (asdf-base-path :story) base))

(defmacro vertical-break (&optional (height "20px"))
  `(html (:div :style ,(format nil "height:~A;" height))))

(defvar *note-lock* (sb-thread:make-mutex))
(defparameter *inhibit-note* nil)

(defmacro with-note-lock (&body body) `(with-mutex (*note-lock*) ,@body))

(defparameter *note-start-clock* (let ((now (get-universal-time)))
                                   (format t "~&;;  Note logging started at: ~A.~%" now)
                                   now))

(defun current-clock-face ()
  (multiple-value-bind (s m h) (get-decoded-time)
    (aref
     #(#\CLOCK_FACE_ONE_OCLOCK
       #\CLOCK_FACE_TWO_OCLOCK
       #\CLOCK_FACE_THREE_OCLOCK
       #\CLOCK_FACE_FOUR_OCLOCK
       #\CLOCK_FACE_FIVE_OCLOCK
       #\CLOCK_FACE_SIX_OCLOCK
       #\CLOCK_FACE_SEVEN_OCLOCK
       #\CLOCK_FACE_EIGHT_OCLOCK
       #\CLOCK_FACE_NINE_OCLOCK
       #\CLOCK_FACE_TEN_OCLOCK
       #\CLOCK_FACE_ELEVEN_OCLOCK
       #\CLOCK_FACE_TWELVE_OCLOCK
       #\CLOCK_FACE_ONE-THIRTY
       #\CLOCK_FACE_TWO-THIRTY
       #\CLOCK_FACE_THREE-THIRTY
       #\CLOCK_FACE_FOUR-THIRTY
       #\CLOCK_FACE_FIVE-THIRTY
       #\CLOCK_FACE_SIX-THIRTY
       #\CLOCK_FACE_SEVEN-THIRTY
       #\CLOCK_FACE_EIGHT-THIRTY
       #\CLOCK_FACE_NINE-THIRTY
       #\CLOCK_FACE_TEN-THIRTY
       #\CLOCK_FACE_ELEVEN-THIRTY
       #\CLOCK_FACE_TWELVE-THIRTY)
     (1- (+ (if (< m 30) 0 12) (if (> h 12) (- h 12) h))))))

(defparameter *show-note-clock* nil)

(defun note (control &rest arguments)
  (unless *inhibit-note*
    (let ((*print-pretty* nil))
      (sb-thread:with-mutex (*note-lock*)
        (apply #'format t
               (format nil "~~&;; ~@[ ~A ~]~A ~A~~%"
                       (and *show-note-clock* (blue (princ-to-string (current-clock-face)) :effect :bright))
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

(defun run-program-to-string (program args)
  (with-output-to-string (str)
    (sb-ext:run-program program args :output str :error str :search t)))

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

(defun starts-with-char (string char)
  (char= char (char string 0)))

(defmacro vertical-break (&optional (height "20px"))
  `(html (:div :style ,(format nil "height:~A;" height))))

(defmacro comment (text)
  `(html (fmt "<!--~%~%~A~%~%-->" ,text)))

(defun last1 (list)
  (car (last list)))
