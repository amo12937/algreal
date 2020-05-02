package amo.euclidea.alpha

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.geometry.figures.{ Line, Point }
import amo.geometry.commands.{ LineCommand, GetVerticalBisectorCommandProvider }
import amo.geometry.problems.Cost
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

            val vbProvider = GetVerticalBisectorCommandProvider()

            val commands = Vector(
                (vbProvider.makeCommand(p1, p3), Cost(1, 3)),
                (LineCommand(Line(p1, p6)), Cost(1, 1)),
                (LineCommand(Line(p3, p5)), Cost(1, 1))
            )

            val initEnv = a15.problem.problemL.initialEnvironment
            val resultEnv = commands.foldLeft(initEnv) {
                case (e, (cmd, cost)) => e.applyCommand(cmd, cost)
            }

            a15.answer.fulfill(resultEnv.board) should be(true)
        }
    }
}
