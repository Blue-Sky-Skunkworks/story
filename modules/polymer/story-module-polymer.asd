(defsystem "story-module-polymer"
  :defsystem-depends-on ("story-module-system")
  :class :story-module-system
  :category "none"
  :icon :puzzle-piece
  :description "none"
  :version "0.1"
  :author "unknown"
  :license "unknown"
  :serial t
  :depends-on ("story-modules" "story-module-images")
  :components ((:static-file "story-module-polymer.asd")
               (:file "polymer")
               (:file "demo-polymer")))
