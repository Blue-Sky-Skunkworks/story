(defsystem :FOO
  :serial t
  :depends-on (:story :story-module-polymer)
  :components ((:static-file "FOO.asd")
               (:file "package")
               (:file "initialize")))
