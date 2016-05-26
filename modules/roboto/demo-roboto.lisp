(in-package :story)

(define-demo roboto ((:roboto))
  (describe-font-folder (story-modules-file "roboto/fonts/") stream))

(defun font-information (filename)
  (zpb-ttf:with-font-loader (font filename)
    (nconc
     (iter (for name in-vector zpb-ttf::*name-identifiers*)
           (when-let ((val (zpb-ttf:name-entry-value name font)))
             (collect (cons name val))))
     (list
      (cons :glyph-count (zpb-ttf:glyph-count font))
      (cons :italic-angle (zpb-ttf:italic-angle font))
      (cons :underline-thickness (zpb-ttf:underline-thickness font))
      (cons :underline-position (zpb-ttf:underline-position font))
      (cons :fixed-pitch-p (zpb-ttf:fixed-pitch-p font))
      (cons :units/em (zpb-ttf:units/em font))
      (cons :ascender (zpb-ttf:ascender font))
      (cons :descender (zpb-ttf:descender font))))))

(defun font-folder-information (path)
  (iter (for file in (directory-files path))
        (when (equalp (pathname-type file) "ttf")
          (collect (cons file (font-information file))))))

(defun describe-font-folder (path stream)
  (html
    (iter (for (file . info) in (font-folder-information path))
          (htm
           (:h3 (esc (pathname-name file)))
           (:table
            (iter (for (k . v) in info)
                  (htm
                   (:tr (:th :style "text-align:right;padding-right:20px;" (esc (symbol-name k)))
                        (:td (esc (princ-to-string v)))))))))))
