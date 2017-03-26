
(defsystem :story
  :serial t
  :depends-on ("alexandria" "cl-who" "parenscript" "hunchentoot" "split-sequence" "zpng" "cl-json"
                            "cl-ppcre" "iterate" "local-time" "cl-ansi-text" "cl-uglify-js" "anaphora"
                            "cffi" "printv" "cl-ascii-art" "cl-css" "inferior-shell" "string-case"
                            "hunchensocket")
  :components ((:static-file "story.asd")
               (:file "package")
               (:file "magic")
               (:file "configuration")
               (:file "sharpl")
               (:file "utility")
               (:file "javascript")
               (:file "git")
               (:file "element")
               (:file "define-story")
               (:file "render")
               (:file "server")
               (:file "status-messages")
               (:file "publish")
               (:file "logging")
               (:file "bower")
               (:file "css")
               (:file "new")
	       (:file "demos")
               (:file "initialize")))
