(in-package :story)

(defun list-story-modules (&key with-version)
  (iter (for system in (ql:list-local-systems))
        (when (and (string-starts-with system "story-module-")
                   (not (equal system "story-module-system")))
          (collect
              (if with-version
                  (list (subseq system 12)
                        (parse-float (asdf:component-version (asdf:find-system system))))
                  (subseq system 12))))))
