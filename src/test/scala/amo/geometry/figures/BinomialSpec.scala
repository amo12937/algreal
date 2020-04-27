package amo.geometry.figures

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.algreal.polynomial.Unipoly
import amo.implicits._
import Binomial._

class BinomialSpec extends AnyWordSpec with Matchers {
    "valueAtX" should {
        "" in {
            val ind = Unipoly.ind[BigInt]
            val fxy = x[BigInt] + (y[BigInt]^2)
            val gx = ind - 1

            val actual = fxy.valueAtX(gx)
            val expected = (ind^2) - ind + 1

            actual should be(expected)
        }
    }
}
