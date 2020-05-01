package amo.geometry.figures

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.implicits._

class LineLikeSpec extends AnyWordSpec with Matchers {
    "equals" should {
        "return true if the line is same" in {
            val l1: LineLike[AlgReal] = Line(Point(0, 0), Point(2, 0))
            val l2: LineLike[AlgReal] = Line(Point(2, 0), Point(0, 0))

            val actual = l1 == l2

            val expected = true
            actual should be(expected)
        }
    }

    "hashCode" should {
        "return true if the line is same" in {
            val sqrt3 = AlgReal(3).sqrt
            val l1: LineLike[AlgReal] = Line(Point(0, 0), Point(1, sqrt3))
            val l2: LineLike[AlgReal] = Line(Point(2, 2 * sqrt3), Point(3, 3 * sqrt3))

            l1 == l2 should be(true)
            l1.a.hashCode should be(l2.a.hashCode)
            l1.b.hashCode should be(l2.b.hashCode)
            l1.c.hashCode should be(l2.c.hashCode)
            l1.interval.left.hashCode should be(l2.interval.left.hashCode)
            l1.interval.right.hashCode should be(l2.interval.right.hashCode)

            l1.hashCode should be(l2.hashCode)
        }
    }
}
