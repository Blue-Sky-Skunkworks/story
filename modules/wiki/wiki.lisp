(in-package :story)

(define-story-module wiki)

(defvar *wiki-directory* (story-modules-file "wiki/sample-wiki/"))

(defun wiki-page-filename (name)
  (format nil "~A~A.md" *wiki-directory* (substitute #\- #\space name)))

(defun wiki-page-exists (name)
  (probe-file (wiki-page-filename name)))

(defun load-wiki-page (name)
  (when (wiki-page-exists name)
    (slurp-file (wiki-page-filename name))))

(defun list-wiki-links (name)
  (let (rtn (text (load-wiki-page name)))
    (do-scans (start end rstart rend "\\[\\[([a-zA-Z0-9\\s]*)\\]\\]" text)
      (push (subseq text (aref rstart 0) (aref rend 0)) rtn))
    (nreverse rtn)))

(defun list-wiki-pages ()
  (sort (mapcar #L((substitute #\space #\- (pathname-name %)))
                (remove-if-not #L(equal (pathname-type %) "md")
                               (cl-fad:list-directory *wiki-directory*)))
        #'string<))






