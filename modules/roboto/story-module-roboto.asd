(defsystem "story-module-roboto"
  :defsystem-depends-on ("story-module-system")
  :class :story-module-system
  :category "none"
  :icon :puzzle-piece
  :description "none"
  :version "0.1"
  :author "unknown"
  :license "unknown"
  :serial t
  :depends-on ("story-modules" "zpb-ttf")
  :components ((:static-file "story-module-roboto.asd")
               (:file "roboto")
               (:file "demo-roboto")))
