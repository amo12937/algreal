package amo

import amo.AlgReal.{ BigInteger, StrumExtension, Unipoly }
import amo.AlgReal.Field.QuotientField
import amo.util.Random

object implicits
    extends BigInteger.implicits
    with QuotientField.implicits
    with Random.implicits
    with StrumExtension.implicits
    with Unipoly.implicits
