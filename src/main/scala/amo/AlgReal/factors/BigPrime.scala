package amo.AlgReal.factors

import amo.AlgReal.{ Closure, RingTrait, Unipoly }
import amo.AlgReal.Field.{ PrimeField, PrimeFieldTrait }

object BigPrime {
    def partitions[T](nn: Int, xxs: Vector[T]): Iterator[(Vector[T], Vector[T])] = (nn, xxs) match {
        case (0, xs) => Iterator((Vector.empty, xs))
        case (_, Vector()) => Iterator()
        case (n, x +: xs) =>
            partitions(n - 1, xs).map({ case (s, rest) => (x +: s, rest) }) ++
            partitions(n, xs).map({ case (s, rest) => (s, x +: rest) })
    }

    def oneNorm[T](f: Unipoly[T])(
        implicit ring: RingTrait[T], ordering: Ordering[T]
    ): T = {
        f.cs.foldLeft(ring.zero) { (acc, c) =>
            if (ordering.lt(c, ring.zero)) ring.sub(acc, c) else ring.add(acc, c)
        }
    }

    def coprimeModP(f: Unipoly[BigInt], g: Unipoly[BigInt])(
        implicit pf: PrimeFieldTrait
    ): Boolean = {
        val f2 = f.mapCoeff(pf.create)
        val g2 = g.mapCoeff(pf.create)
        (f2.gcd(g2)).degree == Closure(0)
    }
}
