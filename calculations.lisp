(in-package :vicentino-tunings)

;;;; This is sort of a scratch book for temporary calculations.

;;; To program the TLA tuning device for the Arciorgano

(defun tla-tables ()
  (calculate-cent-table '(:c :c♯ :d :e♭ :e :f :f♯ :g :g♯ :a :b♭ :b♮)
                        '(:c :c♯/d♭ :d :e♭/d♯ :e :f :f♯/g♭ :g :g♯/a♭ :a :b♭/a♯ :b♮)
                        :tuning3)

  (calculate-cent-table '(:d♭ :d♯ :e♯ :g♭ :a♭ :a♯ :b♯)
                        '(:c♯/d♭ :e♭/d♯ :f  :f♯/g♭ :g♯/a♭ :b♭/a♯ :c2)
                        :tuning3)

  (calculate-cent-table '(:ċ :ċ♯ :ḋ :ė♭ :ė :ḟ :ḟ♯ :ġ :ġ♯ :ȧ :ḃ♭ :ḃ♮)
                        '(:c :c♯/d♭ :d :e♭/d♯ :e :f :f♯/g♭ :g :g♯/a♭ :a :b♭/a♯ :b♮)
                        :tuning3)

  (calculate-cent-table '(:ḋ♭ :ḋ♯ :ġ♭ :ȧ♭ :ȧ♯)
                        '(:c♯/d♭ :e♭/d♯ :f♯/g♭ :g♯/a♭ :b♭/a♯)
                        :tuning3))


;;; Calculations concerning Quintenschaukel

(defvar fifths-c-d '(:c :g :d :a :e :b♮ :f♯ :c♯ :g♯ :d♯ :a♯ :e♯ :b♯ :ġ♭ :ḋ♭ :ȧ♭ :ė♭ :ḃ♭ :ḟ :ċ :ġ :ḋ))

;; fifth tempering in quintenschaukel
(defparameter *magical-fifth* (expt (* 9/8 (expt 2 12)) 1/21))
(defparameter *magical-tempering* (get-tempering (expt (* 9/8 (expt 2 12)) 1/21) 3/2))


;;; Table generation
(let ((reference-pitch 440)
      (tuning-a :tuning1)
      (tuning-b :12ed2)
      (pitch-list (reverse (list :d :e :f :g :a :b♮ :c))))
  (flet ((calculate-frequency (tuning root-note note)
           (* (* reference-pitch (vicentino-tunings:interval tuning :a :down root-note))
              (vicentino-tunings:interval tuning root-note :up note))))
    (format t "~{~%| ~a | ~,3f | ~,3f |~}"
            (do ((remaining-pitch-list pitch-list (rest remaining-pitch-list))
                 (result nil))
                ((null remaining-pitch-list) (reverse result))
              (push (first remaining-pitch-list) result)
              (push (calculate-frequency tuning-a :c (first remaining-pitch-list))
                    result)
              (push (calculate-frequency tuning-b :c (first remaining-pitch-list))
                    result)))))
