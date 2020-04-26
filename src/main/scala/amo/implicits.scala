package amo

import amo.AlgReal.{ AlgReal => AR, BigInteger, Closure, StrumExtension, Unipoly }
import amo.AlgReal.Field.QuotientField
import amo.util.Random

object implicits
    extends AR.implicits
    with BigInteger.implicits
    with Closure.implicits
    with QuotientField.implicits
    with Random.implicits
    with StrumExtension.implicits
    with Unipoly.implicits
