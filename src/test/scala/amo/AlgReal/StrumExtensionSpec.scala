package amo.AlgReal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.AlgReal.Field.QuotientField
import amo.implicits._

class StrumExtensionSpec extends AnyWordSpec with Matchers {
    "variance" should {
        "return a number of times the sign is changed" in {
            val actual = StrumExtension.variance(Vector(1, 0, -1, 0, -1, 0, 1))
            val expected = 2
            actual should be(expected)
        }

        "return 0 if empty list supplied" in {
            val actual = StrumExtension.variance(Vector())
            val expected = 0
            actual should be(expected)
        }

        "return 0 if there's only one element in the list" in {
            val actual = StrumExtension.variance(Vector(1))
            val expected = 0
            actual should be(expected)
        }
    }
}

class UnipolyStrumExtensionSpec extends AnyWordSpec with Matchers {
    val x = Unipoly.ind[BigInt]
    "intervalsWithSign" should {
        "return the same value after the value of function is 0" in {
            val f = 4 * x - 5

            val actual = f.intervalsWithSign(1, Interval(1, 2)).take(5).toVector

            val expected = Vector[Interval[QuotientField[BigInt]]](
                Interval(1, 2),
                Interval(1, rational.create(3, 2)),
                Interval(rational.create(5, 4), rational.create(5, 4)),
                Interval(rational.create(5, 4), rational.create(5, 4)),
                Interval(rational.create(5, 4), rational.create(5, 4)),
            )
            actual should be(expected)
        }

        "return list of aproximate values" in {
            val f = (x^2) - 2

            val actual = f.intervalsWithSign(1, Interval(1, 2)).take(5).toVector

            val expected = Vector[Interval[QuotientField[BigInt]]](
                Interval(1, 2),
                Interval(1, rational.create(3, 2)),
                Interval(rational.create(5, 4), rational.create(3, 2)),
                Interval(rational.create(11, 8), rational.create(3, 2)),
                Interval(rational.create(11, 8), rational.create(23, 16)),
            )
            actual should be(expected)
        }
    }

    "rootBound" should {
        "return 0 for Unipoly.zero" in {
            val actual = Unipoly.zero[BigInt].rootBound
            val expected = rational.zero
            actual should be(expected)
        }

        "return 1 for constant Unipoly" in {
            val actual = Unipoly.one[BigInt].rootBound
            val expected = rational.one
            actual should be(expected)
        }

        "return upper bound in which root exists" in {
            val f = (x^2) - 2
            val actual = f.rootBound
            val expected = rational.create(3)
            actual should be(expected)
        }
    }
}
