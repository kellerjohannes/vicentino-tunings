(in-package :vicentino-tunings)

(defmacro meantone (fraction)
  "Returns a function to calculate the pitches within a meantone scale based on its fifth-index. The
meantone system is defined by FRACTION which describes the amount of tempering the fifth in the
unit of the syntonic comma."
  `(lambda (index)
     (linear-system index :generator-interval (temper 3/2 ,fraction))))
