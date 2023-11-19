(in-package :vicentino-tunings)

;; Functions for calculating pitch

(defun ratio->length (ratio &key (unit-interval (expt 2 1/1200)))
  "Transforms a ratio (or floating point number) representing an interval into a length,where the
UNIT-INTERVAL (default 1 ¢) can be set."
  (/ (log ratio) (log unit-interval)))

(defun simplify (interval &key (identity-interval 2/1) (number-of-iterations 0))
  "Returns an interval (a ratio or a floating point number) that fits between 1 and
IDENTITY-INTERVAL (default 2/1)."
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
  "Returns an interval (ratio or floating point number) by multiplying the
GENERATOR-INTERVAL INDEX times, and puts the result between 1 and IDENTITY-INTERVAL."
  (simplify (expt generator-interval index) :identity-interval identity-interval))

(defun ed-division (index &key (division 12) (identity-interval 2/1))
  "Returns an interval (floating point number)."
  (expt identity-interval (* index (/ 1 division))))

(defun chain-intervals (interval-ratio index)
  "Returns the result of a chain of INTERVAL-RATIO with length INDEX. The result is equivalent to
the function LINEAR-SYSTEM, but in CHAIN-INTERVALS each member of the chain is simplified to
avoid large numbers."
  (do ((result 1 (simplify (* result interval-ratio)))
       (counter index (1- counter)))
      ((zerop counter) result)))

(defun temper (interval amount &key (reference-interval 81/80))
  "Returns an interval (ratio or float) that is modified from INTERVAL by a fraction (AMOUNT) of
the REFERENCE-INTERVAL (default is the syntonic comma, 81/80)."
  (* interval (expt reference-interval amount)))

(defun get-tempering (interval-ratio-real interval-ratio-utopia &key (unit 81/80))
  "Returns the amount of tempering for INTERVAL-RATIO-REAL in relation to the untempered
INTERVAL-RATIO-UTOPIA. The UNIT can be specified (default is the syntonic comma)."
  (ratio->length (/ interval-ratio-real interval-ratio-utopia) :unit-interval unit))

(defun tune (tuning-fun index)
  "Takes a function that calculates a pitch based on an INDEX (normally a fifth-index) and returns
the pitch. See also the functions PITCH and NOTE."
  (unless (functionp tuning-fun)
    (error "Expected a function for :tuning-fun, but ~s is not of type COMPILED-FUNCTION."
           tuning-fun))
  (unless (integerp index)
    (error "Expected an integer for :index, but ~s does not fulfill INTEGERP." index))
  (funcall tuning-fun index))



;; Connecting tuning functions and keymaps

(defun print-keymaps ()
  (format t "~&The following keymaps are available, use these keywords to reference them:")
  (dolist (keymap *keymaps*)
    (format t "~&- ~s" (first keymap))))

(defun get-fifth-index (keymap-id notename)
  "Returns the fifth-index from a keymap that is specified with KEYMAP-ID. To see available keymaps
use PRINT-KEYMAPS."
  (let ((result (cdr (assoc notename (get-keymap keymap-id)))))
    (if result
        result
        (error "Notename ~s couldn't be found. There is nothing to be done to save this situation."
               notename))))

(defun get-pitch (tuning-fun keymap-name notename)
  "Returns the pitch of a NOTENAME (given as keyword) based on a TUNING-FUN and a
KEYMAP-NAME (use PRINT-KEYMAPS to see available keymaps)."
  (let ((index-or-relation (get-fifth-index keymap-name notename)))
    (let ((result (if (integerp index-or-relation)
                      (tune tuning-fun index-or-relation)
                      (case (first index-or-relation)
                        (:relation
                         (simplify (* (second index-or-relation)
                                      ;; obsolete: this old version can't deal with nested :relations
                                      ;; The new version (recursive) needs more testing
                                      ;; (tune tuning-fun (get-fifth-index keymap-name
                                      ;;                                   (third index-or-relation)))
                                      (get-pitch tuning-fun keymap-name (third index-or-relation))
                                      )))
                        (otherwise (error "Index relation ~a unknown." (first index-or-relation)))))))
        (if result
            result
            (error "Couldn't compute the pitch for ~s." notename)))))

(defun pitch-fun (tuning-fun keymap-name)
  "Returns a function that calculates a pitch for a notename (the function's argument) based on a
given TUNING-FUN and a KEYMAP-NAME."
  (lambda (notename) (get-pitch tuning-fun keymap-name notename)))

(defun pitch (pitch-fun notename)
  "Returns the pitch (ratio or float) for a notename (given as a keyword) based on a pitch calculating
function (PITCH-FUN, generated for example with PITCH-FUN). PITCH is similar to TUNE, the
difference is that TUNE finds the elemente in a scale based on an index and PITCH finds it based
on a notename. NOTE adds another layer of abstraction."
  (unless (functionp pitch-fun)
    (error "Expected a function for :pitch-fun, but ~s is not of type COMPILED-FUNCTION."
           pitch-fun))
  (unless (keywordp notename)
    (error "Expected a keyword for :notename, but ~s is not of type KEYWORD." notename))
  (funcall pitch-fun notename))

(defun note (tuning-id notename)
  "Returns the pitch (ratio or float) of a notename (given as a keyword) based on a tuning referenced
by TUNING-ID. Use PRINT-TUNINGS to see available tunings. See also the functions PITCH and TUNE."
  (pitch (get-pitch-fun tuning-id) notename))


;;; Compatibilty with Setzkasten

(defun setzkasten-pitch (tuning-id setzkasten-note)
  "Returns the pitch (ratio or float) of a note in Setzkasten syntax, based on a tuning referenced by
TUNING-ID. Use PRINT-TUNINGS to see available tunings. SETZKASTEN-NOTE needs to be a list of
four elements, the first being a root notename (given as a keyword), the second one the chromatic
alteration (nil, :sharp or :flat), the third one the enharmonic alteration (nil, :dot or :comma) and
the last one a number describing the octave (1-5)."
  (* (note tuning-id (lookup-setzkasten-shorthand (list (first setzkasten-note)
                                                        (second setzkasten-note)
                                                        (third setzkasten-note))))
     (expt 2 (fourth setzkasten-note))))




;;; Calculations in the context of 12ed2

(defun cent-deviation-to-eq (interval-ratio eq-notename)
  "Returns a ¢-value describing the interval between INTERVAL-RATIO and the interval from :c to
EQ-NOTENAME (given as a keyword)."
  (- (ratio->length interval-ratio)
     (* 100.0 (cdr (assoc eq-notename *eq-scale*)))))

(defun calculate-cent-table (notename-list eq-equivalent-list tuning-id
                             &key (reference-note-tuning :a) (reference-note-eq :a))
  "Prints a org-mode compatible table to the standard output containing information to program an
electronic tuner based on ¢-tables. The NOTENAME-LIST and EQ-EQUIVALENT-LIST must consist of
keywords. Use PRINT-TUNINGS to see possible values for TUNING-ID."
  (format t "~&~a:~%~%" (get-tuning-description tuning-id))
  (let ((cent-shift (- (cent-deviation-to-eq (pitch (get-tuning tuning-id) reference-note-tuning)
                                             reference-note-eq))))
    (mapcar (lambda (notename eq-equivalent)
              (let ((interval-ratio (pitch (get-tuning tuning-id) notename)))
                (format t "~&| ~a~4,0t | ~,4f~12,0t | ~,2f~22,0t | ~,2f~32,0t | ~a~41,0t |"
                        notename
                        interval-ratio
                        (ratio->length interval-ratio)
                        (+ cent-shift (cent-deviation-to-eq interval-ratio eq-equivalent))
                        eq-equivalent)))
            notename-list
            eq-equivalent-list))
  nil)


;;; Calculating intervals

(defun interval (tuning-id notename-a direction notename-b)
  "Returns the interval between two notes (given as keywords), based on a tuning referenced by
TUNING-ID (keyword) and a value for the DIRECTION (:UP or :DOWN)."
  (unless (member direction '(:up :down))
    (error "Direction argument is expected to be :UP or :DOWN. ~s is neither." direction))
  (let ((note-a (note tuning-id notename-a))
        (note-b (note tuning-id notename-b)))
    (cond ((eq notename-a notename-b)
           (if (eq direction :up) 2/1 1/2))
          ((and (eq direction :up) (> note-b note-a)) (/ note-b note-a))
          ((and (eq direction :down) (< note-b note-a)) (/ note-b note-a))
          ((and (eq direction :up) (< note-b note-a)) (/ (* 2/1 note-b) note-a))
          ((and (eq direction :down) (> note-b note-a)) (/ note-b (* 2/1 note-a)))
          (t (error "This error should not be possible.")))))

(defun interval-size (tuning-id notename-a direction notename-b
                      &optional (unit-interval (expt 2 1/1200)))
  "Returns the absolute logarithmic value of the interval between two notes (NOTENAME-A and NOTENAME-B
are given as keywords) based on a tuning referenced by TUNING-ID and the DIRECTION. The unit for the
size of the interval is given by UNIT-INTERVAL."
  (abs (ratio->length (interval tuning-id notename-a direction notename-b)
                      :unit-interval unit-interval)))
