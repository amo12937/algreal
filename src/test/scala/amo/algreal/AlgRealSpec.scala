package amo.algreal

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._
import amo.algreal.AlgReal.{ Rat, AlgRealPoly, mkAlgReal }
import amo.algreal.factors.Hensel.implicits._
import amo.algreal.polynomial.Unipoly

class AlgRealSpec extends AnyWordSpec with Matchers with TableDrivenPropertyChecks {
    val x = Unipoly.ind[BigInt]

    "+" should {
        "return 5/6 when calculate 1/2 + 1/3" in {
            val actual = Rat(rational(1, 2)) + Rat(rational(1, 3))

            val expected = Rat(rational(5, 6))

            actual should be(expected)
        }

        "return sqrt(2) + 1/16 when calculate sqrt(2) + 1/16" in {
            val sqrt2 = Rat(2).sqrt
            val oneSixteenth = Rat(rational(1, 16))
            val actual = sqrt2 + oneSixteenth

            val expected = mkAlgReal(256 * (x^2) - 32 * x - 511, Interval(1, 2))

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

    "unary_-" should {
        "return negative value" in {
            val actual = -(Rat(1) + Rat(2).sqrt)

            val expected = mkAlgReal((x^2) + 2 * x - 1, Interval(-3, -2))

            actual should be(expected)
        }
    }

    "-" should {
        "return sqrt(2) - sqrt(3) when calculate sqrt(2) - sqrt(3)" in {
            val u = Rat(2).sqrt
            val v = Rat(3).sqrt
            val actual = u - v
            val expected = mkAlgReal((x^4) - 10 * (x^2) + 1, Interval(-1, 0))
            actual should be(expected)
        }

        "return 1 + sqrt(2) - sqrt(3) when calculate (1 + sqrt(2)) - sqrt(3)" in {
            val u = 1 + Rat(2).sqrt
            val v = Rat(3).sqrt
            val actual = u - v
            val expected = mkAlgReal((x^4) - 4 * (x^3) - 4 * (x^2) + 16 * x - 8, Interval(0, 1))
            actual should be(expected)
        }

        "return (1 + sqrt(2))/16 - sqrt(3) when calculate (1 + sqrt(2))/16 - sqrt(3)" in {
            val u = (1 + Rat(2).sqrt) / 16
            val v = Rat(3).sqrt
            val actual = u - v

            val expected = mkAlgReal(
                65536 * (x^4) - 16384 * (x^3) - 392704 * (x^2) + 49216 * x + 585217,
                Interval(rational(-13, 8), -1)
            )
            actual should be(expected)
        }
    }

    "*" should {
        "return 1/6 when calculate 1/2 * 1/3" in {
            val half = Rat(rational(1, 2))
            val oneThird =  Rat(rational(1, 3))
            val actual = half * oneThird

            val expected = Rat(rational(1, 6))

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
            val half = Rat(rational(1, 2))
            val actual = half.inverse

            val expected = Rat(rational(2))

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
            val twoThird = Rat(rational(2, 3))
            val actual = twoThird pow 4

            val expected = Rat(rational(16, 81))

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

    "nthRoot" should {
        "return 2 when calculare sqrt(4)" in {
            val actual = Rat(4).nthRoot(2)

            val expected = Rat(2)

            actual should be(expected)
        }

        "return sqrt(2) when calculate sqrt(2)" in {
            val actual = Rat(2).nthRoot(2)

            val expected = mkAlgReal((x^2) - 2, Interval(1, 2))

            actual should be(expected)
        }
    }

    "floor" should {
        "return BigInt which is the largest number not exceeding X" in {
            val cases = Table(
                ("inputX", "outputI"),
                (AlgReal(0), BigInt(0)),
                (AlgReal(2), BigInt(2)),
                (AlgReal(-1), BigInt(-1)),
                (AlgReal(5, 2), BigInt(2)),
                (AlgReal(-5, 2), BigInt(-3)),
                (AlgReal(5).sqrt, BigInt(2)),
                (-AlgReal(5).sqrt, BigInt(-3))
            )

            forAll(cases) { (inputX, outputI) =>
                val actual = inputX.floor
                actual should be(outputI)
            }
        }
    }

    "hashCode" should {
        "return same value if the definingPolynomial is same" in {
            val a = mkAlgReal((x^2) - 3, Interval(1, 2))
            val b = mkAlgReal((x^2) - 3, Interval(0, 4))
            val c = mkAlgReal((x^3) - 2, Interval(1, 2))
            b.hashCode should be(a.hashCode)
            c.hashCode should be(a.hashCode)

            val s = Set(a)
            s.contains(b) should be(true)
            s.contains(c) should be(false)
        }
    }

    "realRoots" should {
        "return -2, 2 when supplied x^2 - 4" in {
            val f = (x^2) - 4
            val actual = AlgReal.realRoots(f).toSet

            val expected = Set(Rat(2), Rat(-2))

            actual should be(expected)
        }

        "return list of AlgReal value where f becomes zero" in {
            val f = (x^3) - (x^2) - 2 * x + 2
            val actual = AlgReal.realRoots(f).toSet

            val expected = Set(
                Rat(1),
                AlgRealPoly((x^2) - 2, 1, Interval(rational(3, 4), rational(3, 2))),
                AlgRealPoly((x^2) - 2, -1, Interval(rational(-3, 4), rational(-3, 2)))
            )

            actual should be(expected)
        }

        "ignore multiple root" in {
            val f = (x^2) - 2 * x + 1
            val actual = AlgReal.realRoots(f).toSet
            val expected = Set(Rat(1))
            actual should be(expected)
        }

        "return list of AlgReal when coefficient is AlgReal" in {
            val y = Unipoly.ind[AlgReal]
            val f = (y^2) - (Rat(1) + Rat(2).sqrt) * y - Rat(3).sqrt
            val actual = AlgReal.realRoots(f).toSet

            val expected = Set(
                mkAlgReal(Unipoly(9, 0, -18, 12, -5, 4, 2, -4, 1), Interval(-1, 0)),
                mkAlgReal(Unipoly(9, 0, -18, 12, -5, 4, 2, -4, 1), Interval(2, 3))
            )
            actual should be(expected)
        }
    }
}
