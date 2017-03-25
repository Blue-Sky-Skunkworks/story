(in-package :story)

(defun story-modules-file (&optional base)
  (concatenate 'string (asdf-base-path :story-modules) base))

(defun copy-and-replace (read-file write-file &rest replacements)
  "Copy READ-FILE to WRITE-FILE swapping FROM,TO pairs in REPLACEMENTS."
  (let ((data (slurp-file read-file)))
    (with-output-to-file (stream write-file)
      (iter (for (from to) on replacements by #'cddr)
            (setf data (ppcre:regex-replace-all from data to)))
      (princ data stream))))

(defun parse-float (s)
  (let ((val (let ((*read-eval* nil)) (read-from-string s))))
    (etypecase val
      (float val)
      (fixnum (coerce val 'float)))))
