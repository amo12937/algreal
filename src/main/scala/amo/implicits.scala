package amo

import amo.algreal.{ AlgReal, BigInteger, Closure, StrumExtension, Unipoly }
import amo.algreal.Field.QuotientField
import amo.util.Random

object implicits
    extends BigInteger.implicits
    with Closure.implicits
    with QuotientField.implicits
    with Random.implicits
    with StrumExtension.implicits
    with Unipoly.implicits
