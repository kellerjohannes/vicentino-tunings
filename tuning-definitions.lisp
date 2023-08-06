(in-package :vicentino-tunings)

(defparameter *tunings*
  `((:name "Pythagorean diatonic"
     :id :debug
     :description "Simple diatonic scale from F to B♮, for debugging purposes"
     :fun ,(pitch-fun (meantone 0) :diatonic-F-B♮))
    (:name "1/4-SC-meantone, wolf Ė-Ḃ♮"
     :id :tuning1
     :description "Regular meantone, 1/4-comma, from C♭ (Ḃ♮) to D♯♯♯ (Ė)"
     :fun ,(pitch-fun (meantone -1/4) :wolf-Ė-Ḃ♮))
    (:name "1/3-SC-meantone, wolf Ė-Ḃ♮"
     :id :tuning2
     :description "Regular meantone, 1/3-comma, from C♭ (Ḃ♮) to D♯♯♯ (Ė)"
     :fun ,(pitch-fun (meantone -1/3) :wolf-Ė-Ḃ♮))
    (:name "1/4-SC-meantone, fifth-range G♭-Ḃ♯"
     :id :tuning3
     :description "Regular meantone, 1/4-comma with 38 keys, from G♭ to Ḃ♯"
     :fun ,(pitch-fun (meantone -1/4) :mt-38-g♭-ḃ♯))))


(defun print-tunings ()
  "Lists all available tunings."
  (format t "~&The following tunings are available, use these keywords to reference them:")
  (dolist (tuning *tunings*)
    (format t "~&- ~s :: ~a" (getf tuning :id) (getf tuning :description))))

(defun get-tuning (tuning-id)
  "Returns an plist defining a tuning, referenced by TUNING-ID. Use PRINT-TUNINGS to learn which
tunings are available."
  (find tuning-id *tunings* :key (lambda (item) (getf item :id))))

(defun get-pitch-fun (tuning-id)
  "Returns the pitch-function of a tuning referenced by TUNING-ID. Use PRINT-TUNINGS to see
available ids."
  (getf (get-tuning tuning-id) :fun))

(defun get-tuning-description (tuning-id)
  "Returns a string with the description of the tuning referenced with TUNING-ID."
  (getf (find tuning-id *tunings* :key (lambda (item) (getf item :id))) :description))
