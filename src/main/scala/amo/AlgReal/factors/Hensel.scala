package amo.AlgReal.factors

import amo.AlgReal.Field.{ FiniteFieldTrait, PrimeField }
import amo.AlgReal.{ EuclideanDomainTrait, GcdDomainTrait, Unipoly }

class Hensel(
    implicit ff: FiniteFieldTrait[PrimeField],
    edu: EuclideanDomainTrait[Unipoly[PrimeField]],
    ig: GcdDomainTrait[BigInt]
) {
    def mod(m: BigInt, f: Unipoly[BigInt]): Unipoly[BigInt] =
        f.mapCoeff((c) => c % m + (if (c < 0) m else 0))

    def henselLifting2(
        l: BigInt,
        f: Unipoly[BigInt],
        gg: Unipoly[PrimeField],
        hh: Unipoly[PrimeField]
    ): (Unipoly[BigInt], Unipoly[BigInt]) = {
        val (u, ss, tt) = edu.exgcd(gg, hh)
        def tailRec(
            i: BigInt,
            m: BigInt,
            g: Unipoly[BigInt],
            h: Unipoly[BigInt],
            s: Unipoly[BigInt],
            t: Unipoly[BigInt]
        ): (Unipoly[BigInt], Unipoly[BigInt]) = if (i >= l) (g, h) else {
            val m2 = m.pow(2)
            val e = mod(m2, (f - g * h))
            val (q, r) = (s * e).monicDivMod(h)
            val g2 = mod(m2, g + t * e + q * g)
            val h2 = mod(m2, h + r)
            val b = mod(m2, s * g2 + t * h2 - Unipoly.one[BigInt])
            val (c, d) = (s * b).monicDivMod(h2)
            val s2 = mod(m2, s - d)
            val t2 = mod(m2, t - t * b - c * g2)
            tailRec(i * 2, m2, g2, h2, s2, t2)
        }
        tailRec(
            1,
            ff.characteristic(gg.leadingCoefficient),
            gg.mapCoeff(_.toBigInt),
            hh.mapCoeff(_.toBigInt),
            ss.divide(u).mapCoeff(_.toBigInt),
            tt.divide(u).mapCoeff(_.toBigInt),
        )
    }
}
