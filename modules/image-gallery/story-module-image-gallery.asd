(defsystem "story-module-image-gallery"
  :defsystem-depends-on ("story-module-system")
  :class :story-module-system
  :category "none"
  :icon :puzzle-piece
  :description "none"
  :version "0.1"
  :author "unknown"
  :license "unknown"
  :serial t
  :depends-on ("story-modules" "story-module-polymer" "story-module-packery"
                               "story-module-photoswipe")
  :components ((:static-file "story-module-image-gallery.asd")
               (:file "image-gallery")
               (:file "demo-image-gallery")))

