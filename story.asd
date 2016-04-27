(defsystem :story
  :serial t
  :depends-on ("alexandria" "cl-who" "parenscript" "hunchentoot" "split-sequence" "zpng" "cl-json" "cl-ppcre" "iterate" "local-time"
                            "cl-ansi-text")
  :components ((:static-file "story.asd")
               (:file "package")
               (:file "utility")
               (:file "git")
               (:file "story")
               (:file "server")
               (:module "examples"
                        :serial t
                        :components
                        ((:file "demo")))
               (:file "initialize")))
