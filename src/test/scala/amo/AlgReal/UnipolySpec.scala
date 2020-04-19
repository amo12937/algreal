package amo.AlgReal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UnipolySpec extends AnyWordSpec with Matchers {
    object implicits extends BigInteger.implicits with QuotientField.implicits
    import implicits._

    "+" should {
        "add to the other Unipoly" in {
            val l = Unipoly[BigInt](1, 2, 3)
            val r = Unipoly[BigInt](4, 5, 6)
            l + r should be(Unipoly[BigInt](5, 7, 9))
        }
    }

    "unary_-" should {
        "return minus value of the supplied" in {
            val v = Unipoly[BigInt](1, 2, 3)
            -v should be(Unipoly[BigInt](-1, -2, -3))
        }
    }

    "valueAt" should {
        "return 2 if supllied 1 to x + 1" in {
            val f = Unipoly[BigInt](1, 1)
            val actual: BigInt = f.valueAt(1)
            actual should be(2)
        }

        "return 3/2 if supplied 1/2 to x + 1" in {
            val f = Unipoly[BigInt](1, 1)
            val actual = f.valueAt(QuotientField[BigInt](1, 2))
            actual should be(QuotientField[BigInt](3, 2))
        }
    }

    "gcd" should {
        "return gcd of 2 unipolies" in {
            val f = Unipoly[BigInt](-1, 0, 4) // 4x^2 - 1
            val g = Unipoly[BigInt](1, 4, 4) // 4x^2 + 4x + 1
            val expected = Unipoly[BigInt](1, 2) // 2x + 1
            val actual = f gcd g
            actual should be(expected)
        }

        "return f if g is zero" in {
            val f = Unipoly[BigInt](-1, 0, 4) // 4x^2 - 1
            f gcd Unipoly() should be(f)
        }

        "return g if f is zero" in {
            val g = Unipoly[BigInt](-1, 0, 4) // 4x^2 - 1
            Unipoly[BigInt]() gcd g should be(g)
        }
    }
}
