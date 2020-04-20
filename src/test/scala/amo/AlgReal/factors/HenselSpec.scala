package amo.AlgReal.factors

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.AlgReal.implicits._
import amo.AlgReal.Field.PrimeField
import amo.AlgReal.Unipoly

class HenselSpec extends AnyWordSpec with Matchers {
    "henselLifting2" should {
        "lift 5 -> 25" in {
            implicit val F5 = PrimeField.makePrimeField(5)
            implicit val nToF5 = F5.fromInt _
            implicit val F5Unipoly = Unipoly.makeUnipoly[PrimeField]
            implicit val nToF5U = F5Unipoly.fromInt _

            val x = Unipoly.ind[BigInt]
            val y = Unipoly.ind[PrimeField]
            val f = 3 * (x^3) + 2 * (x^2) + x + 3
            val g = 3 * (y^2) + y + 4
            val h = y + 2
            f.mapCoeff(F5.create) should be(g * h)

            val hensel = new Hensel

            val (g2, h2) = hensel.henselLifting2(2, f, g, h)
            g2 should be(3 * (x^2) + x + 9)
            h2 should be(x + 17)
        }
    }
}
