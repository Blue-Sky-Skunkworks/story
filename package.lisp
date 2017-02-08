
(defpackage story-js
  (:use common-lisp parenscript)
  (:export js-file id set-html set-html* console))

(defpackage story-css
  (:use common-lisp iterate cl-who cl-css)
  (:export style))

(defpackage story
  (:use common-lisp cl-who iterate hunchentoot local-time split-sequence cffi printv cl-ascii-art)
  (:import-from story-js id set-html set-html* console)
  (:import-from parenscript ps ps* @ new create)
  (:import-from ppcre scan do-scans create-scanner)
  (:import-from anaphora aand acond it)
  (:import-from alexandria with-output-to-file when-let if-let with-input-from-file assoc-value with-gensyms ensure-list once-only)
  (:import-from uiop directory-files subdirectories directory-exists-p hostname delete-directory-tree)
  (:export
   *story* *deploy-changed-stories* initialize-story define-story story
   server toggle-production
   html script script*
   vertical-break comment image
   parenscript ps ps* @ new create
   note starts-with-char use-naked-repl
   publish create-story stories describe-story render-current-story))

