package amo.geometry.commands

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.geometry.figures.Point
import amo.implicits._

class VerticalBisectorCommandSpec extends AnyWordSpec with Matchers {
    "equals" should {
        "return true if its fig is same as fig of LineCommand" in {
            val v = new VerticalBisectorCommand[AlgReal](
                Point(0, 0), Point(2, 0)
            )
            val l = new LineCommand[AlgReal](
                Point(1, 1), Point(1, -1)
            )
            val actual = v == l
            actual should be(true)
        }
    }
}
