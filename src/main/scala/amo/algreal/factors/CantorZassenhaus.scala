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
        val q = ff.order(f.leadingCoefficient) // q = 3
        val ind = Unipoly.ind[T]

        //          1       x              x^10 - 1
        //          2       x^3            x^8 + x^6 + x^4 + x^2 + 1
        def tailRec(k: Int, u: Unipoly[T], g: Unipoly[T]): Iterator[(Int, Unipoly[T])] = {
            if (g.degreeInt == 0) Iterator.empty
            else if (g.degreeInt < 2 * k) Iterator((g.degreeInt, g))
            else {
                // 1:  x^3 mod (x^10 - 1) = x^3
                // 2:  x^9 = (x^10 - 1)*0 + x^9
                val u2 = edu.powMod(u, q, f)
                // 1: x^10 - 1 = (x^3 - x)(x^7 + x^5 + x^3 + x) + x^2 - 1
                // 2: x^8 + x^6 + x^4 + x^2 + 1 gcd x^9 - x
                //    x^9 - x = (x^8 + x^6 + x^4 + x^2 + 1)x - x^7 - x^5 - x^3 - 2x
                //    x^8 + x^6 + x^4 + x^2 + 1 = (x^7 + x^5 + x^3 + 2x)x - x^2 + 1
                //
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

    def equalDegreeFactorizationOne(d: Int, h: Unipoly[T], hoge: Unipoly[T]): Option[Unipoly[T]] = {
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

    def equalDegreeFactorization(d: Int, h: Unipoly[T], hoge: Unipoly[T] = Unipoly.one[T]): Iterator[Unipoly[T]] =
        if (h.degreeInt == 0) Iterator.empty
        else if (h.degreeInt == d) Iterator(h)
        else for {
            m <- Iterator.continually(equalDegreeFactorizationOne(d, h, hoge)).flatten.take(1)
            g <- equalDegreeFactorization(d, m, hoge) ++ equalDegreeFactorization(d, h.divide(m), hoge)
        } yield g

    def factor(f: Unipoly[T]): Iterator[Unipoly[T]] = for {
        (d, h) <- distinctDegreeFactorization(f)
        g <- equalDegreeFactorization(d, h, f)
    } yield g
}

