package amo

import amo.algreal.{ AlgReal, BigInteger, Closure }
import amo.algreal.field.{ QuotientField, QuotientFieldOrderingExtension }
import amo.algreal.polynomial.{ StrumExtension, Unipoly, AlgRealExtension }
import amo.geometry.figures.IntersectionExtension
import amo.util.Random

object implicits
    extends AlgReal.implicits
    with AlgRealExtension.implicits
    with BigInteger.implicits
    with Closure.implicits
    with IntersectionExtension.implicits
    with QuotientField.implicits
    with QuotientFieldOrderingExtension.implicits
    with Random.implicits
    with StrumExtension.implicits
    with Unipoly.implicits
