package amo.geometry.problems

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.geometry.commands.VerticalBisectorCommand
import amo.geometry.figures.{ Circle, Point, Line, LineLike, LineSegment }
import amo.implicits._

class BoardSpec extends AnyWordSpec with Matchers {
    "boardHistory.contains" should {
        "return true if the board already exists" in {
            val a = AlgReal(3).nthRoot(3) // 3^(1/3) ≒ 1.442
            val p1: Point[AlgReal] = Point(0, 0)
            val p2: Point[AlgReal] = Point(a, 0)
            val p3: Point[AlgReal] = Point(a, 1)
            val p4: Point[AlgReal] = Point(0, 1)
            val l1: LineLike[AlgReal] = LineSegment(p1, p2)
            val l2: LineLike[AlgReal] = LineSegment(p2, p3)
            val l3: LineLike[AlgReal] = LineSegment(p3, p4)
            val l4: LineLike[AlgReal] = LineSegment(p4, p1)

            val initBoard =
                Board(Set(p1, p2, p3, p4), Set(l1, l2, l3, l4))

            val command1 = new VerticalBisectorCommand(p1, p2)
            val command2 = new VerticalBisectorCommand(p3, p4)

            val board1 = command1.run(initBoard)

            val boardHistory = Set(board1)

            val board2 = command2.run(board1)

            boardHistory.contains(board2) should be(true)
        }
    }
}
