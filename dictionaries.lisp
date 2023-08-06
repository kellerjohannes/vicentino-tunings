(in-package :vicentino-tunings)

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
  '((:diatonic-F-B♮ ((:Ḃ♮ . 5)
                     (:E . 4)
                     (:A . 3)
                     (:D . 2)
                     (:G . 1)
                     (:C . 0)
                     (:F . -1)))
    (:wolf-Ė-Ḃ♮ (
                 ;; only for debugging, need to be solved properly

                 (:C-ʼ . 32)

                 (:Aʼ . 31)
                 (:B♮ʼ . 30)
                 (:B♭ʼ . 29)
                 (:Cʼ . 28)
                 (:Dʼ . 27)
                 (:Eʼ . 26)
                 (:Fʼ . 25)
                 (:Gʼ . 24)

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
                 (:G♭ . -6)
                 (:C♭ . -7)
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
  (let ((result (cadr (assoc name *keymaps*))))
    (if result
        result
        (error "Keymap ~s couldn't be found. There is nothing to do to save this situation."
               name))))

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
