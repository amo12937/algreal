package amo.algreal.factors

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._
import amo.algreal.{ Prime, Unipoly }
import amo.algreal.Field.PrimeField

class BigPrimeSpec extends AnyWordSpec with Matchers {
    "oneNorm" should {
        "return sum of list of absolute coefficients of a Unipoly" in {
            val x = Unipoly.ind[BigInt]
            val f = 3 * (x^2) - 2 * x + 1
            BigPrime.oneNorm(f) should be(6)
        }

        "return 0 if supplied Unipoly.zero" in {
            BigPrime.oneNorm(iUnipoly.zero) should be(0)
        }
    }

    "maxNorm" should {
        "return maximum absolute coefficient of a Unipoly" in {
            val x = Unipoly.ind[BigInt]
            val f = 2 * (x^2) - 3 * x + 1
            BigPrime.maxNorm(f) should be(3)
        }
    }

    "factorCoefficientBound" should {
        "return 13944156602510523416463735259136 for x^100 - 1" in {
            val x = Unipoly.ind[BigInt]
            val f = (x^100) - 1
            val expected = BigInt("13944156602510523416463735259136")
            val actual = BigPrime.factorCoefficientBound(f)
            actual should be(expected)
        }
    }

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

    "coprimeModP" should {
        "return true if it is disjoint on mod p" in {
            val p5 = Prime(5).get
            val F5Implicits = PrimeField.makeImplicits(p5)
            import F5Implicits._
            val x = Unipoly.ind[BigInt]
            val f = (x^4) + 1
            BigPrime.coprimeModP(f, f.diff) should be(true)
        }

        "return false if it is not disjoint on mod p" in {
            val p5 = Prime(5).get
            val F5Implicits = PrimeField.makeImplicits(p5)
            import F5Implicits._
            val x = Unipoly.ind[BigInt]
            val f = (x^5) + 1
            BigPrime.coprimeModP(f, f.diff) should be(false)
        }
    }
}
