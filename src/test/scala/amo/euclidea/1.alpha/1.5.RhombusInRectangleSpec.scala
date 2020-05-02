package amo.euclidea.alpha

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.geometry.figures.Point
import amo.geometry.commands.{ GetLineCommand, GetVerticalBisectorCommand }
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

            val commands = Vector(
                GetVerticalBisectorCommand(p1, p3),
                GetLineCommand(p1, p6),
                GetLineCommand(p3, p5)
            )

            val initEnv = a15.problem.problemL.initialEnvironment
            val resultEnv = commands.foldLeft(initEnv)((e, c) => e.applyCommand(c))

            a15.answer.fulfill(resultEnv.board) should be(true)
        }
    }
}
