(in-package :vicentino-tunings)

(defparameter *tunings*
  `((:name "Pythagorean diatonic"
     :id :debug
     :description "Simple diatonic scale from F to B♮, for debugging purposes"
     :fun ,(pitch-fun (meantone 0) :diatonic-F-B♮))
    (:name "1/4-SC-meantone, wolf Ė-Ḃ♮, pure fifths in sesto ordine"
     :id :tuning1
     :description "Diese Stimmung besteht aus einer regelmässigen $\\frac{1}{4}$-Komma-Mitteltönigkeit mit einer Quintenkette von C♭ (Ḃ♮) bis D♯♯♯ (Ė). Der \\emph{sesto ordine} besteht aus den Tasten Aʼ B♮ʼ B♭ʼ Cʼ Dʼ Eʼ Fʼ und Gʼ, ausserdem ist die eine aussergewöhnliche Tonhöhe definiert, die 3:2 tiefer als G klingt und C-ʼ genannt wird. Diese Stimmung berücksichtigt sämtliche Tasten und Noten, die in den Kapiteln b5-c8 bis b5-c38 erwähnt werden."
     :fun ,(pitch-fun (meantone -1/4) :wolf-Ė-Ḃ♮))
    ;; Here comes :tuning2 (adaptive-just)
    (:name "Regular meantone, 1/3-comma, with Quintenschaukel"
     :id :tuning3
     :description "Diese Stimmung besteht aus einer regelmässigen $\\frac{1}{3}$-Komma-Mitteltönigkeit mit einer Quintenkette von G♭ bis Ḃ♭. Der /sesto ordine/ besteht aus reinen Quinten über dem /primo ordine/, der /quarto ordine/ besteht aus reinen Quinten zum /sesto ordine/."
     :fun ,(pitch-fun (meantone -1/3) :quintenschaukel))
    (:name "1/3-SC-meantone, wolf Ė-Ḃ♮"
     :id :tuning4
     :description "Regular meantone, 1/3-comma, from C♭ (Ḃ♮) to D♯♯♯ (Ė)"
     :fun ,(pitch-fun (meantone -1/3) :wolf-Ė-Ḃ♮))
    (:name "1/4-SC-meantone, fifth-range G♭-Ḃ♯"
     :id :tuning5
     :description "Regular meantone, 1/4-comma with 38 keys, from G♭ to Ḃ♯"
     :fun ,(pitch-fun (meantone -1/4) :mt-38-g♭-ḃ♯))
    (:name "Equal division of the octave in 12 parts"
     :id :12ed2
     :description "Standard equal temperament, 12ed2"
     :fun ,(pitch-fun (equal-system 12) :std-12ed2))
    (:name "Equal division of the octave in 31 parts"
           :id :31ed2
           :description "31-equal, note names expressed with double accidentals, range G♭♭ to A♯♯")))


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
