package amo.AlgReal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._
import amo.AlgReal.factors.Hensel.implicits._

class AlgRealSpec extends AnyWordSpec with Matchers {
    val x = Unipoly.ind[BigInt]
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
