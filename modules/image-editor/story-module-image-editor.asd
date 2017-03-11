(defsystem "story-module-image-editor"
  :defsystem-depends-on ("story-module-system")
  :class :story-module-system
  :category "none"
  :icon :puzzle-piece
  :description "none"
  :version "0.1"
  :author "unknown"
  :license "unknown"
  :serial t
  :depends-on ("story-modules" "story-module-fabric")
  :components ((:static-file "story-module-image-editor.asd")
               (:file "image-editor")
               (:file "demo-image-editor")))

