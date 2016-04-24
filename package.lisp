
(defpackage story
  (:use common-lisp cl-who iterate hunchentoot local-time split-sequence)
  (:import-from parenscript ps ps*)
  (:import-from ppcre scan do-scans)
  (:import-from alexandria with-output-to-file when-let if-let with-input-from-file assoc-value with-gensyms ensure-list)
  (:export define-story))

(defpackage story-js
  (:use common-lisp parenscript)
  (:export js-file))

