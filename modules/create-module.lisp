(in-package :story)

(defun create-story-module (name &key (base (story-modules-file))
                                   (icon :puzzle-piece)
                                   (category "none")
                                   (description "none")
                                   (version 0.1)
                                   (author "unknown")
                                   (license "unknown")
                                   dependencies)
  "Create a new Story module from templates."
  (assert (and icon category description version author license))
  (ensure-directories-exist (format nil "~A~(~A~)/" base name))
  (let ((asd-filename-template (format nil "~Abasis/story-module-foo.asd.template" base))
        (asd-filename (format nil "~A~(~A~)/story-module-~(~A~).asd" base name name))
        (lisp-filename-template (format nil "~Abasis/foo.lisp.template" base))
        (lisp-filename (format nil "~A~(~A~)/~(~A~).lisp" base name name)))
    (cond
      ((probe-file asd-filename) (warn "Module ~S already exists at ~S." name asd-filename))
      ((probe-file lisp-filename) (warn "Module ~S lisp file already exists at ~S." name lisp-filename))
      (t
       (copy-and-replace asd-filename-template asd-filename
                         "FOO" (string-downcase name)
                         "CATEGORY" category
                         "ICON" (string-downcase (prin1-to-string icon))
                         "DESCRIPTION" description
                         "VERSION" (princ-to-string version)
                         "AUTHOR" author
                         "LICENSE" license
                         "DEPENDENCIES" (format nil "~{ ~S~}" dependencies))
       (copy-and-replace lisp-filename-template lisp-filename "FOO" (string-downcase name))
       (format t "Module ~S created." name)))))

(export 'create-story-module)
