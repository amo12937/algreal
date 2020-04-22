package amo.AlgReal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.AlgReal.Field.QuotientField
import amo.implicits._

class UnipolySpec extends AnyWordSpec with Matchers {
    "+" should {
        "add to the other Unipoly" in {
            val l = Unipoly[BigInt](1, 2, 3)
            val r = Unipoly[BigInt](4, 5, 6)
            l + r should be(Unipoly[BigInt](5, 7, 9))
        }
    }

    "addT" should {
        "add T value to the Unipoly" in {
            val l = Unipoly[BigInt](1, 2, 3)
            val r = BigInt(4)
            l.addT(r) should be(Unipoly[BigInt](5, 2, 3))
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

    "composition" should {
        "return f(g(x))" in {
            val f = Unipoly[BigInt](1, 2, 3)  // 3x^2 + 2x + 1
            val g = Unipoly[BigInt](1, 1)  // x + 1
            f.composition(g) should be(Unipoly[BigInt](6, 8, 3))  // f(g(x))
            g.composition(f) should be(Unipoly[BigInt](2, 2, 3))  // g(f(x))
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

    "implicits" should {
        "allow us to add T value to the Unipoly" in {
            val f = Unipoly[BigInt](0, 1) // x
            val t = BigInt(1)
            f + t should be(Unipoly[BigInt](1, 1))
        }
    }
}
