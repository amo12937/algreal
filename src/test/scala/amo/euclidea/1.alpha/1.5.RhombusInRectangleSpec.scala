package amo.euclidea.alpha

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.geometry.figures.{ Line, Point }
import amo.geometry.commands.{ LineCommand, VerticalBisectorCommand }
import amo.geometry.problems.Cost
import amo.implicits._

class Alpha_1_5_RhombusInRectangleSpec extends AnyWordSpec with Matchers {
    "problemL" should {
        "" in {
            val a = AlgReal(3)
            val b = ((a^2) + 1)/(2 * a)
            val c = a - b

            val p1: Point[AlgReal] = Point(0, 0)
            val p3: Point[AlgReal] = Point(a, 1)
            val p5: Point[AlgReal] = Point(b, 0)
            val p6: Point[AlgReal] = Point(c, 1)

            val commands = Vector(
                (new VerticalBisectorCommand(p1, p3), Cost(1, 3)),
                (new LineCommand(p1, p6), Cost(1, 1)),
                (new LineCommand(p3, p5), Cost(1, 1))
            )

            val problemL = Alpha_1_5_RhombusInRectangle.problem[AlgReal].problemL
            val initEnv = problemL.initialEnvironment
            val resultEnv = commands.foldLeft(initEnv) {
                case (e, (cmd, cost)) => e.applyCommand(cmd, cost)
            }

            problemL.answer.fulfill(resultEnv.board) should be(true)
        }
    }
}
