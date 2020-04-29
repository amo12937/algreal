package amo.geometry.figures

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.implicits._

class TwoLineSolverSpec extends AnyWordSpec with Matchers {
    "TwoLineSolver" should {
        "return a point if there's intersecion" in {
            val l1: Line[AlgReal] = Line(Point(0, 0), Point(2, 4))
            val l2: Line[AlgReal] = Line(Point(0, 2), Point(1, 0))
            val actual = l1.intersects(l2).toVector

            val expected = Vector[Point[AlgReal]](Point(AlgReal(1, 2), 1))
            actual should be(expected)
        }
    }

    "LineCircleSolver" should {
        "return intersect points when the line is parallel to y axis" in {
            val l: Line[AlgReal] = Line(Point(0, 0), Point(0, 1))
            val c: Circle[AlgReal] = Circle(Point(1, 0), 2)
            val actual = l.intersects(c).toSet

            val expected = Set[Point[AlgReal]](
                Point(0, -AlgReal(3).sqrt),
                Point(0, AlgReal(3).sqrt),
            )
            actual should be(expected)
        }

        "return intersect points when the line is parallel to x axis" in {
            val l: Line[AlgReal] = Line(Point(0, 2), Point(2, 0))
            val c: Circle[AlgReal] = Circle(Point(0, 0), AlgReal(2).sqrt)
            val actual = l.intersects(c).toSet

            val expected = Set[Point[AlgReal]](
                Point(1, 1)
            )
            actual should be(expected)
        }

        "return empty points when the line did not cross to the circle" in {
            val l: Line[AlgReal] = Line(Point(0, 2), Point(2, 0))
            val c: Circle[AlgReal] = Circle(Point(0, 0), 1)
            val actual = l.intersects(c).toSet

            val expected = Set[Point[AlgReal]]()
            actual should be(expected)
        }
    }

    "CircleLineSolver" should {
        "return a point if there's intersecion" in {
            val c: Circle[AlgReal] = Circle(Point(1, 0), 2)
            val l: Line[AlgReal] = Line(Point(0, 0), Point(0, 1))
            val actual = c.intersects(l).toSet

            val expected = Set[Point[AlgReal]](
                Point(0, -AlgReal(3).sqrt),
                Point(0, AlgReal(3).sqrt),
            )
            actual should be(expected)
        }
    }
}
