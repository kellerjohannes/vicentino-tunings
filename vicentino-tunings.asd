(asdf:defsystem "vicentino-tunings"
  :depends-on (:cl-svg)
  :serial t
  :components ((:file "package")
               (:file "tuning-functions")))
