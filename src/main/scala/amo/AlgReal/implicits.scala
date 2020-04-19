package amo.AlgReal

import amo.AlgReal.Field.QuotientField

object implicits
    extends BigInteger.implicits
    with QuotientField.implicits
    with Unipoly.implicits
