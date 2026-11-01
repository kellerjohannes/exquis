(asdf:defsystem "exquis"
  :depends-on (:alexandria :incudine :clog)
  :serial t
  :components ((:file "package")
               (:file "scratch")))
