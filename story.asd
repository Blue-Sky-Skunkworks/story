(defsystem :story
  :serial t
  :depends-on ("alexandria" "cl-who" "parenscript" "hunchentoot" "split-sequence" "zpng" "cl-json" "cl-ppcre" "iterate" "local-time"
                            "cl-ansi-text")
  :components ((:static-file "story.asd")
               (:file "package")
               (:file "configuration")
               (:file "javascript")
               (:file "utility")
               (:file "git")
               (:file "element")
               (:file "define-story")
               (:file "render")
               (:file "server")
               (:file "logging")
               (:file "bower")
               (:module "examples"
                        :serial t
                        :components
                        (; (:file "demos")
                         ))
               (:file "initialize")))
