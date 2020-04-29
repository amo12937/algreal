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
}
