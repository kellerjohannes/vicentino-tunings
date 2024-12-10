(asdf:defsystem "vicentino-tunings"
  :depends-on (:cl-svg :alexandria)
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "dictionaries")
               (:file "tuning-functions")
               (:file "lookup-tables")
               (:file "tuning-definitions")
               (:file "calculations")))
