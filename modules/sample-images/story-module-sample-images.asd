(defsystem "story-module-sample-images"
  :defsystem-depends-on ("story-module-system")
  :class :story-module-system
  :category "none"
  :icon :puzzle-piece
  :description "none"
  :version "0.1"
  :author "unknown"
  :license "unknown"
  :serial t
  :depends-on ("story-modules" "vecto")
  :components ((:static-file "story-module-sample-images.asd")
               (:file "words")
               (:file "sample-images")
               (:file "demo-sample-images")))
