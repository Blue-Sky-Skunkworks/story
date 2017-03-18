(in-package :story)

(defparameter *repository* (story-file))

(defun git (command &key (repository *repository*))
  (run/lines `(git ,(f "--git-dir=~A.git" (ensure-trailing-slash repository))
                   ,@command)))

(defun git-latest-commit (&key (branch "master") (repository *repository*) )
  (car (git `("log" "-1" "--pretty=format:%H" ,branch) :repository repository)))

(defun git-fetch (&key (what "origin") (repository *repository*))
  (git `("fetch" ,what) :repository repository))

(defun git-list-tags (&key (repository *repository*))
  (git `("tag") :repository repository))

(defun git-hash-object (pathname)
  (first (git `("hash-object" ,pathname))))

(defun git-head ()
  (first (git '("rev-parse" "HEAD"))))

(defun git-object-type (id)
  (first (git `("cat-file" "-t" ,id))))

(defun git-object-size (id)
  (values (parse-integer (first (git `("cat-file" "-s" ,id))))))

(defun git-object-contents (id)
  (git `("cat-file" ,(git-object-type id),id)))

(defun git-list-files ()
  (git '("ls-files")))

(defun git-list-config ()
  (git '("config" "--list")))

