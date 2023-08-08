(defpackage :vicentino-tunings
  (:use :cl)
  (:export setzkasten-pitch
           interval
           interval-size
           ratio->length
           print-tunings
           get-tuning-description))

(in-package :vicentino-tunings)
