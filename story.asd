(defsystem :story
  :serial t
  :depends-on ("alexandria" "cl-who" "parenscript" "hunchentoot" "split-sequence" "zpng" "cl-json"
                            "cl-ppcre" "iterate" "local-time" "cl-ansi-text" "cl-uglify-js" "anaphora"
                            "cffi" "printv" "cl-ascii-art" "cl-css")
  :components ((:static-file "story.asd")
               (:file "package")
               (:file "magic")
               (:file "configuration")
               (:file "javascript")
               (:file "sharpl")
               (:file "utility")
               (:file "git")
               (:file "element")
               (:file "define-story")
               (:file "render")
               (:file "server")
               (:file "publish")
               (:file "logging")
               (:file "bower")
               (:file "images")
               (:file "css")
	       (:file "demos")
               (:file "initialize")))
