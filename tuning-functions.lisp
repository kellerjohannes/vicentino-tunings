(in-package :vicentino-tunings)

(defun ratio->length (ratio &key (unit-interval (expt 2 1/1200)))
  (/ (log ratio) (log unit-interval)))

(defun simplify (interval &key (identity-interval 2/1) (number-of-iterations 0))
  (cond ((< interval 1/1)
         (simplify (* interval identity-interval)
                   :identity-interval identity-interval
                   :number-of-iterations (1+ number-of-iterations)))
        ((>= interval identity-interval)
         (simplify (/ interval identity-interval)
                   :identity-interval identity-interval
                   :number-of-iterations (1+ number-of-iterations)))
        (t (values interval number-of-iterations))))

(defun linear-system (index &key (generator-interval 3/2) (identity-interval 2/1))
  (simplify (expt generator-interval index) :identity-interval identity-interval))

(defun chain-intervals (interval-ratio index)
  (do ((result 1 (simplify (* result interval-ratio)))
       (counter index (1- counter)))
      ((zerop counter) result)))

(defun temper (interval amount &key (reference-interval 81/80))
  (* interval (expt reference-interval amount)))

(defun get-tempering (interval-ratio-real interval-ratio-utopia &key (unit 81/80))
  (ratio-to-cent (/ interval-ratio-real interval-ratio-utopia)
                 :unit unit))

(defmacro meantone (fraction)
  `(lambda (index)
     (linear-system index :generator-interval (temper 3/2 ,fraction))))

(defun tune (tuning-fun index)
  (funcall tuning-fun index))

(defparameter *dict-setzkasten-shorthand*
  '((:c nil nil          :C)
    (:b :sharp nil       :C♯)
    (:b :sharp :comma    :C♯❜)
    (:c nil :dot         :Ċ)
    (:c :flat :dot       :Ċ♭)
    (:c :flat nil        :C♭)
    (:c nil :comma       :C❜)
    (:c :sharp :comma    :C♯❜)
    (:d nil nil          :D)
    (:c :sharp nil       :C♯)
    (:d :flat nil        :D♭)
    (:d nil :dot         :Ḋ)
    (:d :flat :dot       :Ḋ♭)
    (:d nil :comma       :D❜)
    (:e nil nil          :E)
    (:e :flat nil        :E♭)
    (:d :sharp nil       :D♯)
    (:e nil :dot         :Ė)
    (:e :flat :dot       :Ė♭)
    (:e nil :comma       :E❜)
    (:f nil nil          :F)
    (:f nil :comma       :F❜)
    (:e :sharp nil       :E♯)
    (:f nil :dot         :Ḟ)
    (:g nil nil          :G)
    (:f :sharp nil       :F♯)
    (:f :flat nil        :F♭)
    (:f :flat :dot       :Ḟ♭)
    (:g :flat nil        :G♭)
    (:g nil :dot         :Ġ)
    (:g :flat :dot       :Ġ♭)
    (:g nil :comma       :G❜)
    (:g :flat :comma     :G♭❜)
    (:a nil nil          :A)
    (:g :sharp nil       :G♯)
    (:a :flat nil        :A♭)
    (:a nil :dot         :Ȧ)
    (:a :flat :dot       :Ȧ♭)
    (:a nil :comma       :A❜)
    (:a :flat :comma     :A♭❜)
    (:a :flat :dot-comma :Ȧ♭❜)
    (:b nil nil          :B♮)
    (:b :natural nil     :B♮)
    (:b :flat nil        :B♭)
    (:b :flat :comma     :B♭❜)
    (:b :flat :dot-comma :Ḃ♭❜)
    (:b nil :dot-comma   :Ḃ♮❜)
    (:a :sharp nil       :A♯)
    (:b nil :dot         :Ḃ♮)
    (:b :natural :dot    :Ḃ♮)
    (:b :flat :dot       :Ḃ♭)
    (:b nil :comma       :B♮❜)
    ;; doubtful cases, maybe they need separate handling
    (:f :sharp :dot      :G♭)
    (:g :sharp :dot      :A♭)
    (:c :sharp :dot      :D♭)
    ;; (:c :sharp :dot      :Ċ♯)
    ;; (:d :sharp :dot      :Ḋ♯)
    ;; (:f :sharp :dot      :Ḟ♯)
    ;; (:g :sharp :dot      :Ġ♯)
    ;; (:a :sharp :dot      :Ȧ♯)
    ))

(defun lookup-setzkasten-shorthand (setzkasten-pitch)
  (let ((result (fourth (find setzkasten-pitch *dict-setzkasten-shorthand*
                        :key (lambda (entry)
                               (list (first entry) (second entry) (third entry)))
                        :test #'equal))))
    (if result
        result
        (format t "~&Couldn't process note ~a" setzkasten-pitch))))

(defparameter *keymaps*
  '((:wolf-Ė-Ḃ♮ ((:Ė  . 23)
                 (:Ȧ  . 22)
                 (:Ḋ  . 21)
                 (:Ġ  . 20)
                 (:Ċ  . 19)
                 (:Ḟ  . 18)
                 (:Ḃ♭ . 17)
                 (:Ė♭ . 16)
                 (:Ȧ♭ . 15)
                 (:Ḋ♭ . 14)
                 (:Ġ♭ . 13)
                 (:B♯ . 12)
                 (:E♯ . 11)
                 (:A♯ . 10)
                 (:D♯ . 9)
                 (:G♯ . 8)
                 (:C♯ . 7)
                 (:F♯ . 6)
                 (:B♮ . 5)
                 (:E  . 4)
                 (:A  . 3)
                 (:D  . 2)
                 (:G  . 1)
                 (:C  . 0)
                 (:F  . -1)
                 (:B♭ . -2)
                 (:E♭ . -3)
                 (:A♭ . -4)
                 (:D♭ . -5)
                 (:G♭ . -6)
                 (:Ḃ♮ . -7)))
    (:mt-38-g♭-ḃ♯     ((:Ḃ♯ . 31)
                       (:Ė♯ . 30)
                       (:Ȧ♯ . 29)
                       (:Ḋ♯ . 28)
                       (:Ġ♯ . 27)
                       (:Ċ♯ . 26)
                       (:Ḟ♯ . 25)
                       (:Ḃ♮ . 24)
                       (:Ė  . 23)
                       (:Ȧ  . 22)
                       (:Ḋ  . 21)
                       (:Ġ  . 20)
                       (:Ċ  . 19)
                       (:Ḟ  . 18)
                       (:Ḃ♭ . 17)
                       (:Ė♭ . 16)
                       (:Ȧ♭ . 15)
                       (:Ḋ♭ . 14)
                       (:Ġ♭ . 13)
                       (:B♯ . 12)
                       (:E♯ . 11)
                       (:A♯ . 10)
                       (:D♯ . 9)
                       (:G♯ . 8)
                       (:C♯ . 7)
                       (:F♯ . 6)
                       (:B♮ . 5)
                       (:E  . 4)
                       (:A  . 3)
                       (:D  . 2)
                       (:G  . 1)
                       (:C  . 0)
                       (:F  . -1)
                       (:B♭ . -2)
                       (:E♭ . -3)
                       (:A♭ . -4)
                       (:D♭ . -5)
                       (:G♭ . -6)))))

(defun get-keymap (name)
  (cadr (assoc name *keymaps*)))

(defun get-fifth-index (keymap notename)
  (cdr (assoc notename keymap)))

(defun get-pitch (tuning-fun keymap-name notename)
  (tune tuning-fun (get-fifth-index (get-keymap keymap-name) notename)))

(defun pitch-fun (tuning-fun keymap-name)
  (lambda (notename) (tune tuning-fun (get-fifth-index (get-keymap keymap-name) notename))))

(defun pitch (tuning-fun notename)
  (funcall tuning-fun notename))

(defparameter *tunings*
  `((:name "1/4-SC-meantone, wolf Ė-Ḃ♮"
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


(defun get-tuning (tuning-id)
  (getf (find tuning-id *tunings* :key (lambda (item) (getf item :id))) :fun))

(defun get-tuning-description (tuning-id)
  (getf (find tuning-id *tunings* :key (lambda (item) (getf item :id))) :description))

(defun setzkasten-pitch (tuning-id setzkasten-note)
  (* (pitch (get-tuning tuning-id)
            (lookup-setzkasten-shorthand (list (first setzkasten-note)
                                               (second setzkasten-note)
                                               (third setzkasten-note))))
     (expt 2 (fourth setzkasten-note))))


(defun ratio-to-cent (interval-ratio &key (unit (expt 2 1/1200)))
  (/ (log interval-ratio) (log unit)))


(defparameter *eq-scale*
  '((:c . 0)
    (:c♯ . 1)
    (:d♭ . 1)
    (:c♯/d♭ . 1)
    (:d . 2)
    (:d♯ . 3)
    (:e♭ . 3)
    (:e♭/d♯ . 3)
    (:e . 4)
    (:f . 5)
    (:f♯ . 6)
    (:g♭ . 6)
    (:f♯/g♭ . 6)
    (:g . 7)
    (:g♯ . 8)
    (:a♭ . 8)
    (:g♯/a♭ . 8)
    (:a . 9)
    (:a♯ . 10)
    (:b♭ . 10)
    (:b♭/a♯ . 10)
    (:b♮ . 11)
    (:b . 11)
    (:c2 . 12)))


(defun cent-deviation-to-eq (interval-ratio eq-notename)
  (- (ratio-to-cent interval-ratio)
     (* 100.0 (cdr (assoc eq-notename *eq-scale*)))))

(defun calculate-cent-table (notename-list eq-equivalent-list tuning-id
                             &key (reference-note-tuning :a) (reference-note-eq :a))
  (format t "~&~a:~%~%" (get-tuning-description tuning-id))
  (let ((cent-shift (- (cent-deviation-to-eq (pitch (get-tuning tuning-id) reference-note-tuning)
                                             reference-note-eq))))
    (mapcar (lambda (notename eq-equivalent)
              (let ((interval-ratio (pitch (get-tuning tuning-id) notename)))
                (format t "~&| ~a~4,0t | ~,4f~12,0t | ~,2f~22,0t | ~,2f~32,0t | ~a~41,0t |"
                        notename
                        interval-ratio
                        (ratio-to-cent interval-ratio)
                        (+ cent-shift (cent-deviation-to-eq interval-ratio eq-equivalent))
                        eq-equivalent
                        )))
            notename-list eq-equivalent-list))
  nil)

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
                      :tuning3)



(defvar fifths-c-d '(:c :g :d :a :e :b♮ :f♯ :c♯ :g♯ :d♯ :a♯ :e♯ :b♯ :ġ♭ :ḋ♭ :ȧ♭ :ė♭ :ḃ♭ :ḟ :ċ :ġ :ḋ))



;; fifth tempering in quintenschaukel
(defparameter *magical-fifth* (expt (* 9/8 (expt 2 12)) 1/21))
(defparameter *magical-tempering* (get-tempering (expt (* 9/8 (expt 2 12)) 1/21) 3/2))
