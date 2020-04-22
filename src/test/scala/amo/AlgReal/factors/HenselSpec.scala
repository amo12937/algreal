package amo.AlgReal.factors

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

import amo.implicits._
import amo.AlgReal.Field.PrimeField
import amo.AlgReal.{ Prime, Unipoly }

class HenselSpec extends AnyWordSpec with Matchers {
    val r = new Random
    "henselLifting2" should {
        "lift 5 -> 25" in {
            val p5 = Prime(5).get
            val F5Implicits = PrimeField.makeImplicits(p5)
            import F5Implicits.{ pf => F5, _}

            val x = Unipoly.ind[BigInt]
            val y = Unipoly.ind[PrimeField[M]]
            val f = 3 * (x^3) + 2 * (x^2) + x + 3
            val g = 3 * (y^2) + y + 4
            val h = y + 2
            f.mapCoeff(F5.create) should be(g * h)

            val hensel = new Hensel(r.nextBigInt)

            val (g2, h2) = hensel.henselLifting2(2, f, g, h)
            g2 should be(3 * (x^2) + x + 9)
            h2 should be(x + 17)
        }
    }
}
