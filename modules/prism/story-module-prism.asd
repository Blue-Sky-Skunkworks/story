(defsystem "story-module-prism"
  :defsystem-depends-on ("story-module-system")
  :class :story-module-system
  :category "none"
  :icon :puzzle-piece
  :description "none"
  :version "0.1"
  :author "unknown"
  :license "unknown"
  :serial t
  :depends-on ("story-modules")
  :components ((:static-file "story-module-prism.asd")
               (:file "prism")
               (:file "demo-prism")))

