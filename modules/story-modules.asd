(defsystem "story-modules"
  :description "Modules for Story."
  :version "0.1"
  :author "William Halliburton <whalliburton@gmail.com>"
  :license "GPLv3"
  :serial t
  :depends-on ("story-module-system")
  :components ((:static-file "story-modules.asd")
               (:file "utility")
               (:file "define-module")
               (:file "loader")
               (:file "create-module")))

