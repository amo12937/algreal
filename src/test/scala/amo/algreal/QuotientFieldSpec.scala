package amo.algreal

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._

class RationalNumberSpec
extends AnyWordSpec
with Matchers
with TableDrivenPropertyChecks {
    "rational.apply" should {
        "create rational number" in {
            val cases = Table(
                ("inputN", "inputD", "outputN", "outputD"),
                (1, 2, 1, 2),
                (2, 4, 1, 2),
                (3, 1, 3, 1),
                (-1, 1, -1, 1),
                (1, -1, -1, 1),
                (-1, -1, 1, 1)
            )

            forAll(cases) { (inputN, inputD, outputN, outputD) =>
                val q = rational(inputN, inputD)
                q.num should be(outputN)
                q.denom should be(outputD)
            }
        }
    }
}
