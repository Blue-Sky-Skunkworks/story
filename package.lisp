
(defpackage story-js
  (:use common-lisp parenscript cl-who)
  (:import-from alexandria ensure-list)
  (:export js-file id set-html set-html* console dom create-el-html* remove-node get-prototype-of
           ;; text
           ;; aand
           hide show
           try))

(defpackage story-css
  (:use common-lisp iterate cl-who cl-css)
  (:export style))

(defpackage story
  (:use common-lisp story-js cl-who iterate hunchentoot local-time split-sequence
        cffi printv cl-ascii-art inferior-shell string-case hunchensocket)
  (:import-from sb-int named-lambda)
  (:import-from parenscript ps ps* defpsmacro @ new create getprop)
  (:import-from ppcre scan do-scans create-scanner)
  (:import-from anaphora aand acond it)
  (:import-from alexandria with-output-to-file when-let if-let with-input-from-file assoc-value with-gensyms ensure-list once-only)
  (:import-from uiop directory-files subdirectories directory-exists-p hostname delete-directory-tree)
  (:export
   *story* *deploy-changed-stories* initialize-story define-story story
   server toggle-production
   html script script*
   vertical-break comment
   parenscript ps ps* @ new create
   note use-naked-repl
   starts-with-char starts-with-p ends-with-p
   symb ksymb mkstr
   publish create-story stories describe-story render-current-story slurp-file
   to-json from-json
   bugout))

