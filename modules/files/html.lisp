(defpackage story-parsing
  (:use common-lisp iterate closure-html cxml-stp drakma trivial-download))

(in-package :story-parsing)

(defun page-links (raw)
  (let ((doc (parse raw (make-builder)))
        rtn)
    (stp:do-recursively (a doc)
      (when (and (typep a 'element)
                 (equal (local-name a) "a"))
        (push (list (attribute-value a "href") (string-value a)) rtn)))
    (nreverse rtn)))

(defun page-title (raw)
  (let ((doc (parse raw (make-builder))))
    (stp:do-recursively (a doc)
      (when (and (typep a 'element)
                 (equal (local-name a) "title"))
        (return-from page-title (string-value a))))))

