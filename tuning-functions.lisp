(in-package :vicentino-tunings)

(defun ratio->length (ratio &key (unit-interval (expt 2 1/1200)))
  (/ (log ratio) (log unit-interval)))

(defun simplify (interval &key (identity-interval 2/1))
  (cond ((< interval 1/1) (simplify (* interval identity-interval)
                                    :identity-interval identity-interval))
        ((>= interval identity-interval) (simplify (/ interval identity-interval)
                                                   :identity-interval identity-interval))
        (t interval)))

(defun linear-system (index &key (generator-interval 3/2) (identity-interval 2/1))
  (simplify (expt generator-interval index) :identity-interval identity-interval))

(defun temper (interval amount &key (reference-interval 81/80))
  (* interval (expt reference-interval amount)))

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
    ;; (:c :sharp :dot      :Ċ♯)
    (:d :flat nil        :D♭)
    (:d nil :dot         :Ḋ)
    (:d :flat :dot       :Ḋ♭)
    (:d nil :comma       :D❜)
    (:e nil nil          :E)
    (:e :flat nil        :E♭)
    (:d :sharp nil       :D♯)
    ;; (:d :sharp :dot      :Ḋ♯)
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
    ;; (:f :sharp :dot      :Ḟ♯)
    (:g :flat nil        :G♭)
    (:g nil :dot         :Ġ)
    (:g :flat :dot       :Ġ♭)
    (:g nil :comma       :G❜)
    (:g :flat :comma     :G♭❜)
    (:a nil nil          :A)
    (:g :sharp nil       :G♯)
    ;; (:g :sharp :dot      :Ġ♯)
    (:a :flat nil        :A♭)
    (:a nil :dot         :Ȧ)
    (:a :flat :dot       :Ȧ♭)
    ;; (:a :sharp :dot      :Ȧ♯)
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
                 (:Ḃ♮ . -7)))))

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
     :fun ,(pitch-fun (meantone -1/3) :wolf-Ė-Ḃ♮))))


(defun get-tuning (tuning-id)
  (getf (find tuning-id *tunings* :key (lambda (item) (getf item :id))) :fun))

(defun setzkasten-pitch (tuning-id setzkasten-note)
  (* (pitch (get-tuning tuning-id)
            (lookup-setzkasten-shorthand (list (first setzkasten-note)
                                               (second setzkasten-note)
                                               (third setzkasten-note))))
     (expt 2 (fourth setzkasten-note))))
