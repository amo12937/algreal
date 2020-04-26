package amo.AlgReal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._
import amo.AlgReal.factors.Hensel.implicits._
import amo.AlgReal.AlgReal.{ Rat, AlgRealPoly, mkAlgReal }

class AlgRealSpec extends AnyWordSpec with Matchers {
    val x = Unipoly.ind[BigInt]

    "+" should {
        "return 5/6 when calculate 1/2 + 1/3" in {
            val actual = Rat(rational.create(1, 2)) + Rat(rational.create(1, 3))
            val expected = Rat(rational.create(5, 6))
            actual should be(expected)
        }

        "return (sqrt(2) + sqrt(3)) when calculate sqrt(2) + sqrt(3)" in {
            val sqrt2 = mkAlgReal((x^2) - 2, Interval(1, 2))
            val sqrt3 = mkAlgReal((x^2) - 3, Interval(1, 2))
            val expected = mkAlgReal((x^4) - 10 * (x^2) + 1, Interval(3, 4))
            val actual = sqrt2 + sqrt3

            actual should be(expected)
        }

        "return 1 when calculate (1 - sqrt(2)) + sqrt(2)" in {
            val u = mkAlgReal(((x - 1)^2) - 2, Interval(-1, 0))
            val v = mkAlgReal((x^2) - 2, Interval(1, 2))
            val expected = Rat(1)
            val actual = u + v

            actual should be(a[Rat])
            actual should be(expected)
        }
    }

    "realRoots" should {
        "return list of AlgReal value where f becomes zero" in {
            val f = (x^3) - (x^2) - 2 * x + 2
            val actual = AlgReal.realRoots(f).toSet

            val expected = Set(
                Rat(1),
                AlgRealPoly((x^2) - 2, 1, Interval(rational.create(3, 4), rational.create(3, 2))),
                AlgRealPoly((x^2) - 2, -1, Interval(rational.create(-3, 4), rational.create(-3, 2)))
            )

            actual should be(expected)
        }

        "ignore multiple root" in {
            val f = (x^2) - 2 * x + 1
            val actual = AlgReal.realRoots(f).toSet
            val expected = Set(Rat(1))
            actual should be(expected)
        }
    }
}
