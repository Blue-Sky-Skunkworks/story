(in-package :story)

(define-story-module quicklisp
  :scripts (("ql.js" quicklisp-js))
  :stylesheets (("ql.css" quicklisp-css))
  :files ("ql.json")
  :directories ("software")
  :depends-on (:iron-request :files))

(defun list-quicklisp-packages (&optional uninstalled)
  (let ((all (ql:system-apropos-list "")))
    (if uninstalled (remove-if #'ql-dist:installedp all) all)))

(defun install-all-quicklisp-packages ()
  (mapcar 'ql-dist:install (list-quicklisp-packages t)))

(defun quicklisp-search (str)
  (iter (for (name description req) in *quicklisp-info*)
        (when (or (search str name) (search str description))
          (format t "~24A ~A~%" name description))))

(defun quicklisp-info (name)
  (let ((system (ql-dist:find-system name))
        (location (ql:where-is-system name)))
    (list
     (cons :location location)
     (cons :requires (ql-dist:required-systems system)))))

(defun pathname-fullname (file)
  (f "~A~@[.~A~]" (pathname-name file) (pathname-type file)))

(defun find-readme (base)
  (iter (for file in (directory-files base))
        (when (search "README" (pathname-name file))
          (return (pathname-fullname file)))))

(defun list-quicklisp-info ()
  (let ((dist (first (ql-dist:enabled-dists))))
    (list
     (cons :version (ql-dist:version dist))
     (cons :packages
           (iter (for qlsystem in (list-quicklisp-packages))
                 (let ((system (ignore-errors (asdf:find-system (ql-dist:name qlsystem)))))
                   (when system
                     (let* ((name (asdf:component-name system))
                            (where (asdf:system-source-directory system))
                            (description (asdf:system-description system))
                            (readme (find-readme where))
                            (required (ql-dist:required-systems qlsystem)))
                       (collect
                           (list name
                                 description
                                 (last1 (pathname-directory where))
                                 readme
                                 required
                                 ))))))))))

(defun save-quicklisp-info ()
  (write-to-file (story-file "modules/quicklisp/ql.json")
                 (to-json (list-quicklisp-info))))

(in-package :story-js)

(define-script quicklisp-js

  (defvar *quicklisp-listing*)

  (defun fetch-quicklisp-listing (url callback)
    (if *quicklisp-listing*
        (funcall callback *quicklisp-listing*)
        (request (+ "/" url)
                 (lambda (val)
                   (setf *quicklisp-listing* (eval (+ "(" (@ val response) ")")))
                   (funcall callback *quicklisp-listing*)))))

  (defun to-string (arr)
    ((@ arr join) ""))

  (defun create-table (parent rows &key rowclick class-name (fn (lambda (el) el)))
    (let ((table (create-element "table" parent class-name) raw))
      (loop for row in rows
            do (let ((tr (create-el-html* ("tr" table)))
                     (data (funcall fn row)))
                 (loop for col in data
                       do (create-el-html* ("td" tr) col))
                 (when rowclick
                   (setf (@ tr onclick)
                         (let ((r row))
                           (lambda (ev) (funcall rowclick r)))))))))

  (defun render-quicklisp-listing (div url)
    (let ((parent (id div)))
      (fetch-quicklisp-listing
       url
       (lambda (listing)
         (create-el-html* ("h1" parent) (@ listing version))
         (create-table parent (@ listing packages)
                       :class-name "quicklisp"
                       :rowclick
                       (lambda (el)
                         (console el)
                         (visit-url (+ "/software/" (aref el 2) "/")))
                       :fn (lambda (el)
                             (list
                              (aref el 0)
                              (aref el 1)))))))))

(in-package :story-css)

(defun quicklisp-css ()
  (css
   '(("table.quicklisp tr" :cursor pointer))))

