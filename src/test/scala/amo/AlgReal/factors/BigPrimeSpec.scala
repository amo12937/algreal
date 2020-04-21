package amo.AlgReal.factors

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.AlgReal.implicits._
import amo.AlgReal.Unipoly
import amo.AlgReal.Field.PrimeField

class BigPrimeSpec extends AnyWordSpec with Matchers {
    "partitions" should {
        "return a pair of k conbination of n set and its rest" in {
            val actual = BigPrime.partitions(2, Vector(1, 2, 3, 4, 5)).toVector
            val expected = Vector(
                (Vector(1, 2), Vector(3, 4, 5)),
                (Vector(1, 3), Vector(2, 4, 5)),
                (Vector(1, 4), Vector(2, 3, 5)),
                (Vector(1, 5), Vector(2, 3, 4)),
                (Vector(2, 3), Vector(1, 4, 5)),
                (Vector(2, 4), Vector(1, 3, 5)),
                (Vector(2, 5), Vector(1, 3, 4)),
                (Vector(3, 4), Vector(1, 2, 5)),
                (Vector(3, 5), Vector(1, 2, 4)),
                (Vector(4, 5), Vector(1, 2, 3))
            )

            actual should be(expected)
        }
    }

    "oneNorm" should {
        "return sum of list of absolute of coefficients of a Unipoly" in {
            val x = Unipoly.ind[BigInt]
            val f = 3 * (x^2) - 2 * x + 1
            BigPrime.oneNorm(f) should be(6)
        }

        "return 0 if supplied Unipoly.zero" in {
            BigPrime.oneNorm(iUnipoly.zero) should be(0)
        }
    }

    "coprimeModP" should {
        "return true if it is disjoint" in {
            val F5Implicits = PrimeField.makeImplicits(5)
            import F5Implicits._
            val x = Unipoly.ind[BigInt]
            val f = x + 2
            val g = x + 1
            BigPrime.coprimeModP(f, g) should be(true)
        }
    }
}
