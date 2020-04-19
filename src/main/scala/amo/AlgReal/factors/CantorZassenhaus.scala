package amo.AlgReal.factors

import amo.AlgReal.Field.FiniteFieldTrait
import amo.AlgReal.{ EuclideanDomainTrait, Unipoly }

class CantorZassenhaus[T](rnd: () => T)(
    implicit ff: FiniteFieldTrait[T],
    edu: EuclideanDomainTrait[Unipoly[T]]
) {
    def distinctDegreeFactorization(
        f: Unipoly[T]
    ): Iterator[(Int, Unipoly[T])] = {
        val q = ff.order(f.leadingCoefficient)
        val ind = Unipoly.ind[T]

        def tailRec(k: Int, u: Unipoly[T], g: Unipoly[T]): Iterator[(Int, Unipoly[T])] = {
            if (g.degreeInt == 0) Iterator.empty
            else if (g.degreeInt < 2 * k) Iterator((g.degreeInt, g))
            else {
                val u2 = edu.powMod(u, q, f)
                val h = g.gcd(u2 - ind).toMonic()
                val g2 = g.divide(h)
                val k2 = k + 1
                if (h != Unipoly.one[T]) Iterator((k, h)) ++ tailRec(k2, u2, g2)
                else tailRec(k2, u2, g2)
            }
        }
        tailRec(1, ind, f)
    }

    def randomPolyOfDegreeLessThan(n: Int): Unipoly[T] =
        Unipoly(Iterator.continually(rnd()).take(n).toVector)

    def equalDegreeFactorizationOne(d: Int, h: Unipoly[T]): Option[Unipoly[T]] = {
        val q = ff.order(h.leadingCoefficient)
        val u = randomPolyOfDegreeLessThan(h.degreeInt)
        lazy val one = Unipoly.one[T]
        lazy val v = h.gcd(u).toMonic()
        lazy val w = edu.powMod(u, (q.pow(d) - 1) / 2, h)
        lazy val h2 = h.gcd(w - one).toMonic()
        if (u.isZero) None
        else if (!edu.equiv(v, one)) Some(v)
        else if (edu.equiv(h2, one) || edu.equiv(h2, h)) None
        else Some(h2)
    }

    def equalDegreeFactorization(d: Int, h: Unipoly[T]): Iterator[Unipoly[T]] =
        if (h.degreeInt == 0) Iterator.empty
        else if (h.degreeInt == d) Iterator(h)
        else for {
            m <- Iterator.continually(equalDegreeFactorizationOne(d, h)).flatten.take(1)
            g <- equalDegreeFactorization(d, m) ++ equalDegreeFactorization(d, h.divide(m))
        } yield g

    def factor(f: Unipoly[T]): Iterator[Unipoly[T]] = for {
        (d, h) <- distinctDegreeFactorization(f)
        g <- equalDegreeFactorization(d, h)
    } yield g
}

