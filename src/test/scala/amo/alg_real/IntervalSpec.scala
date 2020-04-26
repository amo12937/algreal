package amo.AlgReal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._

class IntervalSpec extends AnyWordSpec with Matchers {
    "+" should {
        "return the possible range that may contain the result of addition" in {
            val lhs = Interval(BigInt(2), BigInt(5))
            val rhs = Interval(BigInt(1), BigInt(6))
            lhs + rhs should be(Interval(BigInt(3), BigInt(11)))
        }
    }

    "unary_-" should {
        "return the possible range that may contain the result of unary -" in {
            val lhs = Interval(BigInt(2), BigInt(5))
            -lhs should be(Interval(BigInt(-5), BigInt(-2)))
        }
    }
}
