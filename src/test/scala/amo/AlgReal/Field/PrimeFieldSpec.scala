package amo.AlgReal.Field

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.AlgReal.Prime
import amo.implicits._

class PrimeFieldSpec extends AnyWordSpec with Matchers {
    "equiv" should {
        "work well with negative value" in {
            val p3 = Prime(3).get
            val F3Implicits = PrimeField.makeImplicits(p3)
            import F3Implicits.{ pf => F3, _ }
            val zero = F3.create(-3)
            F3.equiv(zero, F3.zero) should be(true)
        }
    }
}
