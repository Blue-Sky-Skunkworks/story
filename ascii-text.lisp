(in-package :story)

(defparameter *ascii-font-directory* "/usr/share/figlet/")
(defvar *ascii-fonts* nil)

(defun find-ascii-font (name)
  (if (member name *ascii-fonts* :test 'equal)
      name
      (error "Unknown Ascii font ~S." name)))

(defun load-ascii-text-fonts ()
  (setf *ascii-fonts*
        (sort
         (iter (for file in (directory-files *ascii-font-directory*))
           (let ((type (pathname-type file)))
             (when (member type '("flf" "tlf") :test 'string=)
               (collect (pathname-name file)))))
         #'string<)))

(load-ascii-text-fonts)

(defparameter *ascii-font* "standard")

(defun ascii-text-fonts ()
  (let ((index 0))
    (iter (for row in (group *ascii-fonts* 4))
      (iter (for name in row)
        (if (string= name *ascii-font*)
            (format t "~2D  ~22A  " (incf index) (cyan name :effect :bright))
            (format t "~2D  ~11A  " (incf index) name)))
      (terpri))))

(defun select-ascii-text-font (name-or-index)
  (setf *ascii-font*
        (typecase name-or-index
          (string (find-ascii-font name-or-index))
          (integer
           (if (or (< name-or-index 1) (> name-or-index (length *ascii-fonts*)))
               (error "Ascii font index out of range.")
               (nth (1- name-or-index) *ascii-fonts*))))))

(defun indent-paragraph (text spaces &optional (char #\space))
  (let ((lines (split-sequence #\newline text))
        (indent (make-string spaces :initial-element char)))
    (with-output-to-string (stream)
      (iter (for els on lines)
        (princ indent stream)
        (princ (car els) stream)
        (when (cdr els) (terpri stream))))))

(defun ascii-text (text &key (font *ascii-font*) (width 80) indent border crop gay metal left right)
  (select-ascii-text-font font)
  (let ((filter (format nil "~{~@[~A~^:~]~}" (list (and border "border")
                                                   (and crop "crop")
                                                   (and gay "gay")
                                                   (and metal "metal")
                                                   (and left "left")
                                                   (and right "right")))))
    (let ((base (run-program-to-string "toilet" (nconc
                                                 (list "-f" *ascii-font* "-w" width)
                                                 (when (plusp (length filter)) (list "-F" filter))
                                                 (list text)))))
      (if indent
          (indent-paragraph base indent)
          base))))

(defun demo-ascii-fonts ()
  (iter (for font in *ascii-fonts* )
    (format t "~A~%~%~A~%~%" (white font) (ascii-text "Hello!" :font font))))
