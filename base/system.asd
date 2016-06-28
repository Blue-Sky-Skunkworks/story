(defsystem :FOO
  :serial t
  :depends-on ()
  :components ((:static-file "FOO.asd")
               (:file "package")
               (:file "initialize")))
