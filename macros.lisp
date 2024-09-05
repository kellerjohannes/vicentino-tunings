(in-package :vicentino-tunings)

(defmacro meantone (fraction)
  "Returns a function to calculate the pitches within a meantone scale based on its fifth-index. The
meantone system is defined by FRACTION which describes the amount of tempering the fifth in the unit
of the syntonic comma. This macro can also be used to calculate Pythagorean systems, by using 0
tempering."
  `(lambda (index)
     (linear-system index :generator-interval (temper 3/2 ,fraction))))

(defmacro equal-system (division)
  "Returns a function to calculate the pitches of a scale that is as result of the equal division of
the octave (or any other interval) in a specific number of parts. The system is defined by the
number of these equal parts (and the identity interval, usually the octave)."
  `(lambda (index)
     (ed-division index :division ,division)))

(defmacro lookup-table (table)
  "Returns a function that looks up a pitch that is given in a table (for example
*LOOKUP-TABLE-PAPE-ARCIORGANO*). Is expects an index (which usually represents a key number)."
  `(lambda (keynr)
     (cdr (assoc keynr ,table))))
