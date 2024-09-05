(defpackage :vicentino-tunings
  (:use :cl)
  (:export setzkasten-pitch
           interval
           interval-size
           ratio->length
           print-tunings
           get-tuning-description
           simplify))

(in-package :vicentino-tunings)
