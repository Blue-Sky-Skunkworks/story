(defsystem "story-module-quicklisp"
  :defsystem-depends-on ("story-module-system")
  :class :story-module-system
  :category "none"
  :icon :puzzle-piece
  :description "none"
  :version "0.1"
  :author "unknown"
  :license "unknown"
  :serial t
  :depends-on ("story-modules" "story-module-polymer")
  :components ((:static-file "story-module-quicklisp.asd")
               (:file "quicklisp")
               (:file "demo-quicklisp")))

