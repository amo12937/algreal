package amo

import amo.AlgReal.{ BigInteger, Unipoly }
import amo.AlgReal.Field.QuotientField
import amo.util.Random

object implicits
    extends BigInteger.implicits
    with QuotientField.implicits
    with Unipoly.implicits
    with Random.implicits
