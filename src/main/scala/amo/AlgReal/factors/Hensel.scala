package amo.AlgReal.factors

import scala.math

import amo.AlgReal.{ EuclideanDomainTrait, Prime, Unipoly }
import amo.AlgReal.Field.{ PrimeField, PrimeFieldModular, PrimeFieldTrait }
import amo.util.Random._

class Hensel(rnd: BigInt => BigInt)(
    implicit edi: EuclideanDomainTrait[BigInt]
) {
    def mod(m: BigInt, f: Unipoly[BigInt]): Unipoly[BigInt] =
        f.mapCoeff((c) => c % m + (if (c < 0) m else 0))

    def henselLifting2[M <: PrimeFieldModular](
        l: Int,
        f: Unipoly[BigInt],
        gg: Unipoly[PrimeField[M]],
        hh: Unipoly[PrimeField[M]]
    )(
        implicit pf: PrimeFieldTrait[M],
        edu: EuclideanDomainTrait[Unipoly[PrimeField[M]]]
    ): (Unipoly[BigInt], Unipoly[BigInt]) = {
        val (u, ss, tt) = edu.exgcd(gg, hh)
        def tailRec(
            i: Int,
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
            pf.characteristic(gg.leadingCoefficient),
            gg.mapCoeff(_.toBigInt),
            hh.mapCoeff(_.toBigInt),
            ss.divide(u).mapCoeff(_.toBigInt),
            tt.divide(u).mapCoeff(_.toBigInt),
        )
    }

    def henselLifting[M <: PrimeFieldModular](
        l: Int,
        f: Unipoly[BigInt],
        gs: Vector[Unipoly[PrimeField[M]]]
    )(
        implicit pf: PrimeFieldTrait[M],
        edu: EuclideanDomainTrait[Unipoly[PrimeField[M]]]
    ): Iterator[Unipoly[BigInt]] = gs match {
        case Vector() => Iterator.empty
        case Vector(g) => {
            val p = pf.characteristic(g.leadingCoefficient)
            val m = p.pow(l)
            val invLcF = edi.inverseMod(f.leadingCoefficient, m)
            Iterator.single(
                mod(m, f.mapCoeff(_ * invLcF))
            )
        }
        case _ => {
            val (gs1, gs2) = gs.splitAt(gs.length / 2)
            val g = Unipoly.product(gs1).scale(pf.create(f.leadingCoefficient))
            val h = Unipoly.product(gs2)
            val (f1, f2) = henselLifting2(l, f, g, h)
            henselLifting(l, f1, gs1) ++ henselLifting(l, f2, gs2)
        }
    }

    def findL(p: BigInt, bound: BigInt): (Int, BigInt) = {
        val b = 2 * bound + 1
        val l = math.ceil(math.log(b.doubleValue) / math.log(p.doubleValue)).toInt
        val m = p.pow(l)
        Iterator.iterate((l, m))({
            case (ll, mm) => (ll + 1, mm * p)
        }).filter(_._2 > b).buffered.head
    }

    def factorWithPrime[M <: PrimeFieldModular](
        p: Prime,
        bound: BigInt,
        f: Unipoly[BigInt]
    ): Iterator[Unipoly[BigInt]] = {
        val pfImplicits = PrimeField.makeImplicits(p)
        import pfImplicits._

        val cz = new CantorZassenhaus(() => pf.create(rnd(p.n)))

        val fP = f.mapCoeff(pf.create).toMonic()
        val factorsP = cz.factor(fP).toVector
        val (l, m) = findL(p.n, bound)
        val factors = henselLifting(l, f, factorsP)
        Iterator.empty
    }

    //def factorCombinationsModulo(m, bound, k, f, factors)

}
