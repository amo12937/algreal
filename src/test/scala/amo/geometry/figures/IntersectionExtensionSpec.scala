package amo.geometry.figures

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.Field.QuotientField
import amo.implicits._

class TwoLineSolverSpec extends AnyWordSpec with Matchers {
    "TwoLineSolver" should {
        "return a point if there's intersecion" in {
            val l1: Line[QuotientField[BigInt]] =
                Line(Point(0, 0), Point(2, 4))
            val l2: Line[QuotientField[BigInt]] =
                Line(Point(0, 2), Point(1, 0))
            val actual = l1.intersects(l2).toVector

            val expected = Vector[Point[QuotientField[BigInt]]](Point(rational(1, 2), 1))
            actual should be(expected)
        }
    }
}
