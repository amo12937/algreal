package amo.AlgReal.factors

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scala.util.Random

import amo.AlgReal.implicits._
import amo.AlgReal.Field.PrimeField
import amo.AlgReal.Unipoly

class BigIntegerSpec extends AnyWordSpec with Matchers {
    val r = new Random
    implicit val F37 = PrimeField.makePrimeField(37)
    implicit val nToF37 = F37.fromInt _
    implicit val F37Unipoly = Unipoly.makeUnipoly[PrimeField]

    def rnd(): PrimeField = F37.create(r.nextInt(37))

    "distinctDegreeFactorization" should {
        "return 2 elements for x^7 - 1" in {
            val cz = new CantorZassenhaus(rnd)

            val f = Unipoly[PrimeField](-1, 0, 0, 0, 0, 0, 0, 1) // x^7 - 1
            val actual = cz.distinctDegreeFactorization(f).take(10).toVector
            val expected = Vector(
                (1, Unipoly[PrimeField](-1, 1)),
                (3, Unipoly[PrimeField](1, 1, 1, 1, 1, 1, 1))
            )

            actual should be(expected)
        }
    }

    "equalDegreeFactorization" should {
        "return 1 element for (1, x - 1)" in {
            val cz = new CantorZassenhaus(rnd)
            val d = 1
            val h = Unipoly[PrimeField](-1, 1)
            val actual = cz.equalDegreeFactorization(d, h).take(10).toVector
            val expected = Vector(Unipoly[PrimeField](-1, 1))
            actual should be(expected)
        }

        "return 2 element for (3, x^6+x^5+x^4+x^3+x^2+x+1)" in {
            val cz = new CantorZassenhaus(rnd)
            val d = 3
            val h = Unipoly[PrimeField](1, 1, 1, 1, 1, 1, 1)
            val actual = cz.equalDegreeFactorization(d, h).take(10).toSet
            val expected = Set(
                Unipoly[PrimeField](36, 28, 29, 1),
                Unipoly[PrimeField](36, 8, 9, 1)
            )
            actual should be(expected)
        }
    }

    "factor" should {
        "factorize" in {
            val r = new Random
            implicit val F37 = PrimeField.makePrimeField(37)
            implicit val nToF37 = F37.fromInt _
            implicit val F37Unipoly = Unipoly.makeUnipoly[PrimeField]

            def rnd(): PrimeField = F37.create(r.nextInt(37))

            val cz = new CantorZassenhaus(rnd)

            val f = Unipoly[PrimeField](-1, 0, 0, 0, 0, 0, 0, 1) // x^7 - 1

            val actual = cz.factor(f).toSet
            val expected = Set[Unipoly[PrimeField]](
                Unipoly(36, 1),         //                 x + 36
                Unipoly(36, 8, 9, 1),   // x^3 +  9x^2 +  8x + 36
                Unipoly(36, 28, 29, 1)  // x^3 + 29x^2 + 28x + 36
            )

            actual should be(expected)
        }
    }
}
