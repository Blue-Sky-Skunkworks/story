(defsystem "story-module-debugger"
  :defsystem-depends-on ("story-module-system")
  :class :story-module-system
  :category "none"
  :icon :puzzle-piece
  :description "none"
  :version "0.1"
  :author "unknown"
  :license "unknown"
  :serial t
  :depends-on ("story-modules" "story-module-polymer" "story-module-files" "story-module-prism")
  :components ((:static-file "story-module-debugger.asd")
               (:file "debugger")
               (:file "present")
               (:file "commands")
               (:file "demo-debugger")))

