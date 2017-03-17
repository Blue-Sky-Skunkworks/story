(defsystem "story-module-files"
  :defsystem-depends-on ("story-module-system")
  :class :story-module-system
  :category "none"
  :icon :puzzle-piece
  :description "none"
  :version "0.1"
  :author "unknown"
  :license "unknown"
  :serial t
  :depends-on ("story-modules" "story-module-images" "story-module-polymer"
                               "story-module-prism"
                               "cl-json"
                               "closure-html" "cxml-stp" "drakma" "trivial-download")
  :components ((:static-file "story-module-files.asd")
               (:file "html")
               (:file "viewer")
               (:file "files")
               (:file "pdf")
               (:file "demo-files")))

