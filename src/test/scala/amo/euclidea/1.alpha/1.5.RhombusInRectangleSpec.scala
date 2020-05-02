package amo.euclidea.alpha

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.geometry.figures.Point
import amo.geometry.commands.{ GetVerticalBisectorCommand }
import amo.implicits._

class Alpha_1_5_RhombusInRectangleSpec extends AnyWordSpec with Matchers {
    "problemL" should {
        "" in {
            val a15 = Alpha_1_5_RhombusInRectangle
            val a = a15.a
            val b = ((a^2) + 1)/(2 * a)
            val c = a - b

            val p1 = a15.p1
            val p3 = a15.p3
            val p5: Point[AlgReal] = Point(b, 0)
            val p6: Point[AlgReal] = Point(c, 1)

            val command1 = GetVerticalBisectorCommand(p1, p3)

            val env1 = a15.problem.problemL.initialEnvironment

            val env2 = env1.applyCommand(command1)
            env2.board.points.contains(p5) should be(true)
            env2.board.points.contains(p6) should be(true)
        }
    }
}
