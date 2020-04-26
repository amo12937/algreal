package amo.algreal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._
import amo.algreal.factors.Hensel.implicits._
import amo.algreal.AlgReal.{ Rat, AlgRealPoly, mkAlgReal }

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
            val actual = sqrt2 + sqrt3

            val expected = mkAlgReal((x^4) - 10 * (x^2) + 1, Interval(3, 4))

            actual should be(expected)
        }

        "return 1 when calculate (1 - sqrt(2)) + sqrt(2)" in {
            val u = mkAlgReal(((x - 1)^2) - 2, Interval(-1, 0))
            val v = mkAlgReal((x^2) - 2, Interval(1, 2))
            val actual = u + v

            val expected = Rat(1)

            actual should be(a[Rat])
            actual should be(expected)
        }
    }

    "*" should {
        "return 1/6 when calculate 1/2 * 1/3" in {
            val half = Rat(rational.create(1, 2))
            val oneThird =  Rat(rational.create(1, 3))
            val actual = half * oneThird

            val expected = Rat(rational.create(1, 6))

            actual should be(expected)
        }

        "return sqrt(6) when calculate sqrt(2) * sqrt(3)" in {
            val sqrt2 = mkAlgReal((x^2) - 2, Interval(1, 2))
            val sqrt3 = mkAlgReal((x^2) - 3, Interval(1, 2))
            val actual = sqrt2 * sqrt3

            val expected = mkAlgReal((x^2) - 6, Interval(2, 3))

            actual should be(expected)
        }

        "return 2 when calculate sqrt(2) * sqrt(2)" in {
            val sqrt2 = mkAlgReal((x^2) - 2, Interval(1, 2))
            val actual = sqrt2 * sqrt2

            val expected = Rat(2)

            actual should be(a[Rat])
            actual should be(expected)
        }
    }

    "inverse" should {
        "return 2 when calculate inverse of 1/2" in {
            val half = Rat(rational.create(1, 2))
            val actual = half.inverse

            val expected = Rat(rational.create(2))

            actual should be(expected)
        }

        "return 1/sqrt(2) when calculate inverse of sqrt(2)" in {
            val sqrt2 = mkAlgReal((x^2) - 2, Interval(1, 2))
            val actual = sqrt2.inverse

            val expected = mkAlgReal(2 * (x^2) - 1, Interval(0, 1))

            actual should be(expected)
        }
    }

    "pow" should {
        "return 16/81 when calculate (2/3)^4" in {
            val twoThird = Rat(rational.create(2, 3))
            val actual = twoThird pow 4

            val expected = Rat(rational.create(16, 81))

            actual should be(expected)
        }

        "return 3363 - 2378 sqrt(2) when calculate (sqrt(2) - 1)^10" in {
            val a = mkAlgReal((x^2) + 2 * x - 1, Interval(0, 1))
            val actual = a pow 10

            val expected = mkAlgReal((x^2) - 6726 * x + 1, Interval(0, 1))

            actual should be(expected)
        }

        "return sqet(2) + 1 when calculate (sqrt(2) - 1)^(-1)" in {
            val a = mkAlgReal((x^2) + 2 * x - 1, Interval(0, 1))
            val actual = a pow (-1)

            val expected = mkAlgReal((x^2) - 2 * x - 1, Interval(2, 3))

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
