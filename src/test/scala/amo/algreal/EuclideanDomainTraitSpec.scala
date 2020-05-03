package amo.algreal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._
import amo.algreal.field.PrimeField
import amo.algreal.polynomial.Unipoly

class PrimeFieldEuclideanDomainSpec extends AnyWordSpec with Matchers {
    "exgcd" should {
        "" in {
            val p5 = Prime(5).get

            val F5Implicits = PrimeField.makeImplicits(p5)
            import F5Implicits.{ pf => F5, pfUnipoly => F5Unipoly, _}

            val x = Unipoly.ind[PrimeField[M]]
            val f = (x^2) + x + 1
            val g = (x^2) - x + 1

            val (u, s, t) = F5Unipoly.exgcd(f, g)
            u should be(Unipoly(F5.create(1)))
            s should be(2*x + 3)
            t should be(3*x + 3)
            (s * f + t * g) should be(u)
        }
    }
}
