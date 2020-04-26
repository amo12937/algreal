package amo.AlgReal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._

class BigIntegerSpec extends AnyWordSpec with Matchers {
    "squareRoot" should {
        "return floor aquare root" in {
            BigInteger.squareRoot(0) should be(0)
            BigInteger.squareRoot(1) should be(1)
            BigInteger.squareRoot(99) should be(9)
            BigInteger.squareRoot(100) should be(10)
            BigInteger.squareRoot(101) should be(10)
            val x = BigInt(Int.MaxValue)
            val n = x.pow(10)
            val expected = x.pow(5)
            val actual = BigInteger.squareRoot(n)
            actual should be(expected)
        }

        "throw error if negative number supplied" in {
            assertThrows[ArithmeticException] {
                BigInteger.squareRoot(-1)
            }
        }
    }

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

    "unit" should {
        "return 1 if a > 0" in {
            bigInt.unit(1) should be(1)
        }

        "return 1 if a == 0" in {
            bigInt.unit(0) should be(1)
        }

        "return -1 if a < 0" in {
            bigInt.unit(-1) should be(-1)
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
