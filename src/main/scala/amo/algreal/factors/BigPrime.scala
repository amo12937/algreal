package amo.algreal.factors

import amo.implicits._
import amo.algreal.{ BigInteger, Closure, RingTrait }
import amo.algreal.field.{ PrimeFieldModular, PrimeFieldTrait }
import amo.algreal.polynomial.Unipoly

object BigPrime {
    def oneNorm[T](f: Unipoly[T])(
        implicit ring: RingTrait[T], ordering: Ordering[T]
    ): T = {
        f.cs.foldLeft(ring.zero) { (acc, c) => ring.add(acc, ring.abs(c)) }
    }

    def maxNorm[T](f: Unipoly[T])(
        implicit ring: RingTrait[T], ordering: Ordering[T]
    ): T = f.cs.map(ring.abs).max

    def factorCoefficientBound(f: Unipoly[BigInt]): BigInt = {
        val n = f.degreeInt
        (BigInteger.squareRoot(n + 2) + 1) * BigInt(2).pow(n) * maxNorm(f)
    }

    def partitions[T](nn: Int, xxs: Vector[T]): Iterator[(Vector[T], Vector[T])] = (nn, xxs) match {
        case (0, xs) => Iterator((Vector.empty, xs))
        case (_, Vector()) => Iterator()
        case (n, x +: xs) =>
            partitions(n - 1, xs).map({ case (s, rest) => (x +: s, rest) }) ++
            partitions(n, xs).map({ case (s, rest) => (s, x +: rest) })
    }

    def coprimeModP[M <: PrimeFieldModular](f: Unipoly[BigInt], g: Unipoly[BigInt])(
        implicit pf: PrimeFieldTrait[M]
    ): Boolean = {
        val f2 = f.mapCoeff(pf.create)
        val g2 = g.mapCoeff(pf.create)
        (f2.gcd(g2)).degree == Closure(0)
    }
}
