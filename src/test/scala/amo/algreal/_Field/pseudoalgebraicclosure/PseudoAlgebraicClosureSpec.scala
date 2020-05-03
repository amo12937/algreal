package amo.algreal.Field.pseudoalgebraicclosure

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.AlgReal
import amo.implicits._

class PseudoAlgebraicClosureSpec extends AnyWordSpec with Matchers {
    "add" should {
        "lazy calculate" in {
            val v: PseudoAlgebraicClosure[AlgReal] = Scalar(AlgReal(1)) + Scalar(AlgReal(2))
            v should be(Add(Scalar(AlgReal(1)), Scalar(AlgReal(2))))
            v.calculate should be(Scalar(AlgReal(3)))
        }

        "lazy calculate with indeterminate value" in {
            val a = Indeterminate("x")
            val b = Scalar(AlgReal(1))
            val v = a + b
            v should be(Add(Indeterminate("x"), Scalar(AlgReal(1))))
            v.calculate should be(Add(Indeterminate("x"), Scalar(AlgReal(1))))
            val u = v.assign(Map(a -> Scalar(AlgReal(2))))
            u should be(Add(Scalar(AlgReal(2)), Scalar(AlgReal(1))))
            u.calculate should be(Scalar(AlgReal(3)))
        }
    }

    "negate" should {
        "return lhs if supplied Negate(lhs)" in {
            val a = Scalar(AlgReal(1))
            val b = Negate(a)
            b should be(Negate(Scalar(AlgReal(1))))
            b.calculate should be(Scalar(AlgReal(-1)))
            val c = Negate(b)
            c should be(Scalar(AlgReal(1)))
        }
    }
}
