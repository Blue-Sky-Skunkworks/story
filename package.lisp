
(defpackage story
  (:use common-lisp cl-who iterate hunchentoot local-time split-sequence)
  (:import-from parenscript ps ps*)
  (:import-from ppcre scan do-scans)
  (:import-from alexandria with-output-to-file when-let if-let with-input-from-file assoc-value with-gensyms ensure-list once-only)
  (:import-from cl-ansi-text with-color black red green yellow blue magenta cyan white)
  (:export define-story))

(defpackage story-js
  (:use common-lisp parenscript)
  (:export js-file))

(require 'story-modules)
