(in-package :story)

(defun create-story-module (name &key (base (story-modules-file))
                                   (icon :puzzle-piece)
                                   (category "none")
                                   (description "none")
                                   (version 0.1)
                                   (author "unknown")
                                   (license "unknown")
                                   dependencies)
  "Create a new Story module."
  (assert (and icon category description version author license))
  (ensure-directories-exist (f "~A~(~A~)/" base name))
  (let ((asd-filename-template (f "~Abasis/story-module-foo.asd.template" base))
        (asd-filename (f "~A~(~A~)/story-module-~(~A~).asd" base name name))
        (lisp-filename-template (f "~Abasis/foo.lisp.template" base))
        (lisp-filename (f "~A~(~A~)/~(~A~).lisp" base name name))
        (demo-filename-template (f "~Abasis/demo-foo.lisp.template" base))
        (demo-filename (f "~A~(~A~)/demo-~(~A~).lisp" base name name)))
    (cond
      ((probe-file asd-filename) (warn "Module ~S already exists at ~S." name asd-filename))
      ((probe-file lisp-filename) (warn "Module ~S lisp file already exists at ~S." name lisp-filename))
      ((probe-file demo-filename) (warn "Module ~S demo file already exists at ~S." name demo-filename))
      (t
       (copy-and-replace asd-filename-template asd-filename
                         "FOO" (string-downcase name)
                         "CATEGORY" category
                         "ICON" (string-downcase (prin1-to-string icon))
                         "DESCRIPTION" description
                         "VERSION" (princ-to-string version)
                         "AUTHOR" author
                         "LICENSE" license
                         "DEPENDENCIES" (f "~{ ~S~}" dependencies))
       (copy-and-replace lisp-filename-template lisp-filename "FOO" (string-downcase name))
       (copy-and-replace demo-filename-template demo-filename "FOO" (string-downcase name))
       ;; (ql:register-local-projects)
       ;; (ql:quickload (f "story-module-~A" (string-downcase name)))
       (format t "Module ~S created." name)))))

(export 'create-story-module)
