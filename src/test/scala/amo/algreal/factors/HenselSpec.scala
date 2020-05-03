package amo.algreal.factors

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

import amo.implicits._
import amo.algreal.field.PrimeField
import amo.algreal.Prime
import amo.algreal.polynomial.Unipoly

class HenselSpec extends AnyWordSpec with Matchers {
    val r = new Random
    val x = Unipoly.ind[BigInt]
    val hensel = new Hensel(r.nextBigInt)

    "henselLifting2" should {
        "lift 5 -> 25" in {
            val p5 = Prime(5).get
            val F5Implicits = PrimeField.makeImplicits(p5)
            import F5Implicits.{ pf => F5, _}

            val y = Unipoly.ind[PrimeField[M]]
            val f = 3 * (x^3) + 2 * (x^2) + x + 3
            val g = 3 * (y^2) + y + 4
            val h = y + 2
            f.mapCoeff(F5.create) should be(g * h)

            val (g2, h2) = hensel.henselLifting2(2, f, g, h)
            g2 should be(3 * (x^2) + x + 9)
            h2 should be(x + 17)
        }

        "lift 5 -> 625" in {
            val p5 = Prime(5).get
            val F5Implicits = PrimeField.makeImplicits(p5)
            import F5Implicits.{ pf => F5, _}

            val y = Unipoly.ind[PrimeField[M]]
            val f = (x^4) + (x^2) + 1
            val g = (y^2) + y + 1
            val h = (y^2) - y + 1

            f.mapCoeff(F5.create) should be(g * h)

            val (g2, h2) = hensel.henselLifting2(4, f, g, h)
            g2 should be((x^2) + x + 1)
            h2 should be((x^2) + 624 * x + 1)
        }
    }

    "factor" should {
        "return x - 1 and x^4 + x^3 + x^2 + x + 1 if x^5 - 1 supplied" in {
            val actual = hensel.factor((x^5) - 1)
            actual.toSet should be(Set(
                (x - 1),
                ((x^4) + (x^3) + (x^2) + x + 1)
            ))
        }

        "work well with x^10 - 1" in {
            val actual = hensel.factor((x^10) - 1)
            actual.toSet should be(Set(
                (x - 1),
                (x + 1),
                ((x^4) + (x^3) + (x^2) + x + 1),
                ((x^4) - (x^3) + (x^2) - x + 1)
            ))
        }

        "work weil with x^3 - x^2 - 2x + 2" in {
            val actual = hensel.factor(-(x^3) + (x^2) + 2 * x - 2)
            actual.toSet should be(Set(
                -Unipoly.one[BigInt],
                x - 1,
                (x^2) - 2
            ))
        }
    }
}
