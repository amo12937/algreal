package amo.AlgReal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class BigIntegerSpec extends AnyWordSpec with Matchers {
    object implicits extends BigInteger.implicits with QuotientField.implicits
    import implicits._

    "bigInt.compare" should {
        "return -1 if x < y" in {
            bigInt.compare(1, 10) should be(-1)
        }

        "return 1 if x > y" in {
            bigInt.compare(10, 1) should be(1)
        }

        "return 0 if x == y" in {
            bigInt.compare(10, 10) should be(0)
        }
    }

    "bigInt.zero" should {
        "be 0" in {
            bigInt.zero should be(0)
        }
    }

    "bigInt.one" should {
        "be 1" in {
            bigInt.one should be(1)
        }
    }

    "bigInt.add" should {
        "return a + b" in {
            bigInt.add(1, 2) should be(3)
        }
    }

    "bigInt.negate" should {
        "return -a" in {
            bigInt.negate(1) should be(-1)
        }
    }

    "bigInt.times" should {
        "return a * b" in {
            bigInt.times(2, 3) should be(6)
        }
    }

    "bigInt.timesN" should {
        "return a * n" in {
            bigInt.timesN(2, 3) should be(6)
        }
    }

    "bigInt.pow" should {
        "return a to the power b" in {
            bigInt.pow(2, 3) should be(8)
        }
    }

    "bigInt.divide" should {
        "return a / b" in {
            bigInt.divide(4, 2) should be(2)
        }

        "truncate the remainder of dividing a by b" in {
            bigInt.divide(5, 2) should be(2)
        }
    }

    "bigInt.gcd" should {
        "return a greatest common divisor" in {
            bigInt.gcd(12, 18) should be(6)
        }
    }

    "bigInt.content" should {
        "return a greatest common divisor of a list" in {
            bigInt.content(Vector(12, 18, 14)) should be(2)
        }
    }
}
