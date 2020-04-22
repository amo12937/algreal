package amo.AlgReal.factors

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.util.Random

import amo.implicits._
import amo.AlgReal.Field.PrimeField
import amo.AlgReal.{ Prime, Unipoly }

class BigIntegerSpec extends AnyWordSpec with Matchers {
    val r = new Random
    val p37 = Prime(37).get
    val F37Implicits = PrimeField.makeImplicits(p37)
    import F37Implicits.{ pf => F37, _}

    def rnd(): PrimeField[M] = F37.create(r.nextInt(37))
    val x = Unipoly.ind[PrimeField[M]]

    "distinctDegreeFactorization" should {
        "return 2 elements for x^7 - 1" in {
            val cz = new CantorZassenhaus(() => rnd())

            val f = (x^7) - 1
            val actual = cz.distinctDegreeFactorization(f).take(10).toVector
            val expected = Vector(
                (1, x - 1),
                (3, (x^6) + (x^5) + (x^4) + (x^3) + (x^2) + x + 1)
            )

            actual should be(expected)
        }
    }

    "equalDegreeFactorization" should {
        "return 1 element for (1, x - 1)" in {
            val cz = new CantorZassenhaus(() => rnd())
            val d = 1
            val h = x - 1
            val actual = cz.equalDegreeFactorization(d, h).take(10).toVector
            val expected = Vector(x - 1)
            actual should be(expected)
        }

        "return 2 element for (3, x^6+x^5+x^4+x^3+x^2+x+1)" in {
            val cz = new CantorZassenhaus(() => rnd())
            val d = 3
            val h = (x^6) + (x^5) + (x^4) + (x^3) + (x^2) + x + 1
            val actual = cz.equalDegreeFactorization(d, h).take(10).toSet
            val expected = Set(
                (x^3) + 29*(x^2) + 28*x + 36,
                (x^3) +  9*(x^2) +  8*x + 36
            )
            actual should be(expected)
        }
    }

    "factor" should {
        "factorize" in {
            val cz = new CantorZassenhaus(() => rnd())

            val f = (x^7) - 1

            val actual = cz.factor(f).toSet
            val expected = Set(
                                      x + 36,
                (x^3) +  9*(x^2) +  8*x + 36,
                (x^3) + 29*(x^2) + 28*x + 36
            )

            actual should be(expected)
        }
    }
}
