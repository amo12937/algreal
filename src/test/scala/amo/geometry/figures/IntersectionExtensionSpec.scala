package amo.geometry.figures

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.implicits._

class TwoLineSolverSpec extends AnyWordSpec with Matchers {
    "TwoLineSolver" should {
        "return a point if there's intersecion" in {
            val l1: LineLike[AlgReal] = Line(Point(0, 0), Point(2, 4))
            val l2: LineLike[AlgReal] = Line(Point(0, 2), Point(1, 0))
            val actual = l1.intersects(l2).toVector

            val expected = Vector[Point[AlgReal]](Point(AlgReal(1, 2), 1))
            actual should be(expected)
        }

        "return empty if line segment does not intersect" in {
            val l1: LineLike[AlgReal] = LineSegment(Point(1, 0), Point(2, 4))
            val l2: LineLike[AlgReal] = LineSegment(Point(0, 1), Point(1, -1))

            val actual = l1.intersects(l2).toVector
            val expected = Vector[Point[AlgReal]]()
            actual should be(expected)
        }

        "return a point with line segment" in {
            val a = AlgReal(3).nthRoot(3)
            val b = ((a^2) + 1) / (2 * a)
            val c = b - a
            val p0: Point[AlgReal] = Point(0, 0)
            val p1: Point[AlgReal] = Point(b, 0)
            val l1: LineLike[AlgReal] = LineSegment(Point(0, 0), Point(a, 0))
            val l2: LineLike[AlgReal] = Line(p1, Point(c, 1))

            l1.innerProductIsInInterval(p1) should be(true)
            l2.innerProductIsInInterval(p1) should be(true)
            l1.intersects(l2).toSet should be(Set(p1))
        }
    }

    "LineCircleSolver" should {
        "return intersect points when the line is parallel to y axis" in {
            val l: LineLike[AlgReal] = Line(Point(0, 0), Point(0, 1))
            val c: Circle[AlgReal] = Circle(Point(1, 0), 2)
            val actual = l.intersects(c).toSet

            val expected = Set[Point[AlgReal]](
                Point(0, -AlgReal(3).sqrt),
                Point(0, AlgReal(3).sqrt),
            )
            actual should be(expected)
        }

        "return intersect points when the line is parallel to x axis" in {
            val l: LineLike[AlgReal] = Line(Point(0, 2), Point(2, 0))
            val c: Circle[AlgReal] = Circle(Point(0, 0), AlgReal(2).sqrt)
            val actual = l.intersects(c).toSet

            val expected = Set[Point[AlgReal]](
                Point(1, 1)
            )
            actual should be(expected)
        }

        "return empty points when the line did not cross to the circle" in {
            val l: LineLike[AlgReal] = Line(Point(0, 2), Point(2, 0))
            val c: Circle[AlgReal] = Circle(Point(0, 0), 1)
            val actual = l.intersects(c).toSet

            val expected = Set[Point[AlgReal]]()
            actual should be(expected)
        }

        "return empty when the line segment does not cross to the circle" in {
            val c: Circle[AlgReal] = Circle(Point(0, 0), 1)
            val l: LineLike[AlgReal] = LineSegment(Point(4, 0), Point(2, 0))

            val actual = l.intersects(c).toSet

            val expected = Set[Point[AlgReal]]()
            actual should be(expected)
        }
    }

    "CircleLineSolver" should {
        "return a point if there's intersecion" in {
            val c: Circle[AlgReal] = Circle(Point(1, 0), 2)
            val l: LineLike[AlgReal] = Line(Point(0, 0), Point(0, 1))
            val actual = c.intersects(l).toSet

            val expected = Set[Point[AlgReal]](
                Point(0, -AlgReal(3).sqrt),
                Point(0, AlgReal(3).sqrt),
            )
            actual should be(expected)
        }
    }

    "twoCircleSolver" should {
        "construct a perpendicular line from a given point to a given line" in {
            val p = Point(AlgReal(1), AlgReal(2))   // 点 p(1, 2)
            val p1 = Point(AlgReal(5), AlgReal(0))
            val p2 = Point(AlgReal(0), AlgReal(-2))
            val l = Line(p1, p2)                    // 点(5, 0), (0, -2) を通る線

            val c = Circle(p, AlgReal(7))           // 中心点p, 半径7 の円

            val cs = c.intersects(l).map(Circle(_, AlgReal(8))).toVector
                                                    // 円c と 直線l の各交点 を中心とする半径 8 の円
            cs.length should be(2)
            val c0 = cs(0)
            val c1 = cs(1)

            val ps = c0.intersects(c1).toVector     // c0, c1 の交点
            ps.length should be(2)
            val p3 = ps(0)
            val p4 = ps(1)

            (p2 - p1).innerProduct(p4 - p3) should be(AlgReal(0))
                                                    // 直線l と p3, p4 を結んだ線は垂直になっている
        }
    }
}
