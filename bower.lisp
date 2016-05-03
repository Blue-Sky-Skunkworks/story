(in-package :story)

(defun load-bower ()
  (from-json (slurp-file (story-file "bower.json"))))

(defun bower ()
  "Describe bower system."
  (iter (for (k . v) in (load-bower))
        (format t "~13A ~A~%" k v)))

(defun list-bower-systems ()
  (iter (for (name) in (assoc-value (load-bower) :dependencies))
        (collect (list name (format nil "~Abower_components/~(~A~)/" (story-file) name)))))

(defun list-bower-documentation ()
  (labels ((recur (base &optional first)
             (nconc (iter (for file in (directory-files base))
                          (when (or (member (pathname-type file) '("md" "txt" "html") :test 'equal)
                                    (and (null (pathname-type file)) (scan "[A-Z]+" (pathname-name file))))
                            (collect file)))
                    (when first (iter (for dir in (subdirectories base))
                                      (appending (recur dir)))))))
    (iter (for (name base) in (list-bower-systems))
          (collect
              (cons name (recur base t))))))
