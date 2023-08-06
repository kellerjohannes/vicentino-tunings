(asdf:defsystem "vicentino-tunings"
  :depends-on (:cl-svg)
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "dictionaries")
               (:file "tuning-functions")
               (:file "tuning-definitions")
               (:file "calculations")))
