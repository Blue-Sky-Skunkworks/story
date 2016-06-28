(defsystem :FOO
  :serial t
  :depends-on (:story)
  :components ((:static-file "FOO.asd")
               (:file "package")
               (:file "initialize")))
