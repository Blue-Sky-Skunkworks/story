(in-package :story)

(defparameter *story-directory* (truename "~/quicklisp/local-projects/"))

(defun create-story (name &key (icon #\black_square) (color "black"))
  "Create a skeletion story called NAME."
  (let* ((name (string-downcase name))
         (story-dir (concatenate 'string (namestring *story-directory*) name "/"))
         (asdf-name (format nil "~A.asd" name)))
    (assert (string/= name "foo"))
    (when (probe-file story-dir)
      (error "Story ~S already exists." name))
    (ensure-directories-exist story-dir)
    (flet ((from (name) (concatenate 'string (asdf-base-path :story) "base/" name))
           (to (name) (concatenate 'string story-dir name)))
      (copy-and-replace (from "system.asd") (to asdf-name) "FOO" name)
      (copy-and-replace (from "initialize.lisp") (to "initialize.lisp") "FOO" name "Foo" (string-capitalize name))
      (copy-and-replace (from "package.lisp") (to "package.lisp") "FOO" name)
      (copy-and-replace (from "utility.lisp") (to "utility.lisp") "FOO" name)
      (copy-and-replace (from "start") (to "start") "FOO" name)
      (copy-and-replace (from ".project") (to ".project") "FOO" name "Foo" (string-capitalize name)
                        "ICON" (prin1-to-string icon) "COLOR" color)
      (sb-posix:chmod (to "start") #o755)
      (copy-and-replace (from "start.lisp") (to "start.lisp"))))
  name)


