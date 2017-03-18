(in-package :story)

(defparameter *story-base-directory* "~/quicklisp/local-projects/")

(defun create-story (name &key (base *story-base-directory*)
                            (icon :puzzle-piece)
                            (category "none")
                            (description "none")
                            (version 0.1)
                            (author "unknown")
                            (license "unknown")
                            dependencies)
  "Create a new Story."
  (assert (and icon category description version author license))
  (let* ((name (string-downcase name))
         (source (f "~Atemplate/" (story-file)))
         (root (f "~A~A/" base name)))
    (when (probe-file root) (error "Story ~S already exists." name))
    (ensure-directories-exist (f "~A~A/" base name))
    (flet ((from (n) (expand-home-path (f "~A~A" source n)))
           (to (n) (expand-home-path (f "~A~A/~A" base name n))))
       (copy-and-replace (from "foo.asd.template") (to (f "~A.asd" name))
                         "FOO" name
                         "DESCRIPTION" description
                         "VERSION" (princ-to-string version)
                         "AUTHOR" author
                         "LICENSE" license
                         "DEPENDENCIES" (f "~{ ~S~}" dependencies))
       (copy-and-replace (from "initialize.lisp") (to "initialize.lisp") "FOO" name "Foo" (string-capitalize name))
       (copy-and-replace (from "package.lisp") (to "package.lisp") "FOO" name)
       (copy-and-replace (from "start") (to "start") "FOO" name)
       (copy-and-replace (from ".project") (to ".project") "FOO" name "Foo" (string-capitalize name)
                         "ICON" (prin1-to-string icon))
       (sb-posix:chmod (to "start") #o755)
       (copy-and-replace (from "start.lisp") (to "start.lisp"))))
  (values))

