package amo

import amo.algreal.{ AlgReal, BigInteger, Closure }
import amo.algreal.Field.QuotientField
import amo.algreal.polynomial.{ StrumExtension, Unipoly }
import amo.util.Random

object implicits
    extends AlgReal.implicits
    with BigInteger.implicits
    with Closure.implicits
    with QuotientField.implicits
    with Random.implicits
    with StrumExtension.implicits
    with Unipoly.implicits
