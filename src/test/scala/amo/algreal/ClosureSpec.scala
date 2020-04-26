package amo.algreal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ClosureSpec extends AnyWordSpec with Matchers {
    import Closure.implicits._

    "<" should {
        "return true if compare 1 to 2" in {
            Closure(1) < Closure(2) should be(true)
        }

        "return false if compare 2 to 1" in {
            Closure(2) < Closure(1) should be(false)
        }

        "return false if compare 2 to 2" in {
            Closure(2) < Closure(2) should be(false)
        }

        "return true if compare 1 to PositiveInfinity" in {
            Closure(1) < Closure.PositiveInfinity should be(true)
        }

        "return false if compare PositiveInfinity to 1" in {
            Closure.PositiveInfinity < Closure(1) should be(false)
        }

        "return false if compare 1 to NegativeInfinity" in {
            Closure(1) < Closure.NegativeInfinity should be(false)
        }

        "return true if compare NegativeInfinity to 1" in {
            Closure.NegativeInfinity < Closure(1) should be(true)
        }

        "return true if compare NegativeInfinity to PositiveInfinity" in {
            Closure.NegativeInfinity < Closure.PositiveInfinity should be(true)
        }

        "return false if compare NegativeInfinity to NegativeInfinity" in {
            Closure.NegativeInfinity < Closure.NegativeInfinity should be(false)
        }

        "return false if compare PositiveInfinity to NegativeInfinity" in {
            Closure.PositiveInfinity < Closure.NegativeInfinity should be(false)
        }

        "return false if compare PositiveInfinity to PositiveInfinity" in {
            Closure.PositiveInfinity < Closure.PositiveInfinity should be(false)
        }
    }
}
