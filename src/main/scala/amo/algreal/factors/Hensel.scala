package amo.algreal.factors

import scala.annotation.tailrec
import scala.math
import scala.util.Random

import amo.implicits._
import amo.algreal.{ EuclideanDomainTrait, Prime }
import amo.algreal.field.{ PrimeField, PrimeFieldModular, PrimeFieldTrait }
import amo.algreal.polynomial.Unipoly
import amo.util.Random._

class Hensel(rnd: BigInt => BigInt)(
    implicit edi: EuclideanDomainTrait[BigInt]
) {
    def mod(m: BigInt, f: Unipoly[BigInt]): Unipoly[BigInt] =
        f.mapCoeff(_ % m match {
            case k if (k < 0) => k + m
            case k => k
        })

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
        ): (Unipoly[BigInt], Unipoly[BigInt]) = {
            if (i >= l) (g, h) else {
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
        }}
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

    /*
     * return (l, m)
     *   where m = p^l, m > 2 * bound + 1
     */
    def findL(p: BigInt, bound: BigInt): (Int, BigInt) = {
        val b = 2 * bound + 1
        val l = math.ceil(math.log(b.doubleValue) / math.log(p.doubleValue)).toInt
        val m = p.pow(l)
        Iterator.iterate((l, m))({
            case (ll, mm) => (ll + 1, mm * p)
        }).filter(_._2 > b).buffered.head
    }

    def factorWithPrime[M <: PrimeFieldModular](
        bound: BigInt,
        f: Unipoly[BigInt]
    )(
        implicit pf: PrimeFieldTrait[M],
        pfUnipoly: EuclideanDomainTrait[Unipoly[PrimeField[M]]]
    ): Iterator[Unipoly[BigInt]] = {
        val p = pf.modular.p
        val cz = new CantorZassenhaus(() => pf.create(rnd(p.n)))

        val fP = f.mapCoeff(pf.create).toMonic()
        val factorsP = cz.factor(fP).toVector
        val (l, m) = findL(p.n, bound)
        val factors = henselLifting(l, f, factorsP).toVector
        factorCombinationsModulo(m, bound, 1, f, factors)
    }

    def factorCombinationsModulo(
        m: BigInt,
        bound: BigInt,
        k: Int,
        f: Unipoly[BigInt],
        factors: Vector[Unipoly[BigInt]]
    ): Iterator[Unipoly[BigInt]] = {
        def toIntegerM(n: BigInt): BigInt = {
            val k = n % m
            if (2 * k > m) k - m else k
        }

        def loop(
            kk: Int, ff: Unipoly[BigInt], fs: Vector[Unipoly[BigInt]]
        ): Iterator[Unipoly[BigInt]] = fs match {
            case Vector() => Iterator.empty
            case _ if (2 * kk > fs.length) => Iterator(ff)
            case _ => BigPrime.partitions(kk, fs).map({ case (s, rest) =>
                val lcF = ff.leadingCoefficient
                val g = Unipoly.product(s).scale(lcF).mapCoeff(toIntegerM)
                val h = Unipoly.product(rest).scale(lcF).mapCoeff(toIntegerM)
                (g, h, rest)
            }).find({ case (g, h, rest) =>
                BigPrime.oneNorm(g) * BigPrime.oneNorm(h) <= bound
            }) match {
                case None => loop(kk + 1, ff, fs)
                case Some((g, h, rest)) =>
                    Iterator(g.primitivePart) ++
                    loop(kk, h.primitivePart, rest)
            }
        }

        loop(k, f, factors)
    }

    def factor(ff: Unipoly[BigInt]): Iterator[Unipoly[BigInt]] = {
        val (c, f) = ff.contentAndPrimitivePart match {
            case (c2, f2) => (c2 * f2.leadingCoefficient.signum, f2.scale(f2.leadingCoefficient.signum))
        }
        val lcF = f.leadingCoefficient
        val bound = BigPrime.factorCoefficientBound(f.scale(lcF))
        Iterator(c).filter(_ != 1).map(Unipoly(_)) ++ (Prime.primes.filter(p => {
            lcF % p.n != 0
        }).map(PrimeField.makeImplicits(_))
        .find(pfImplicits => BigPrime.coprimeModP(f, f.diff)(pfImplicits.pf)) match {
            case None => Iterator.empty
            case Some(pfImplicits) => {
                import pfImplicits._
                factorWithPrime(bound, f)
            }
        })
    }
}

object Hensel {
    trait implicits {
        val r: Random
        implicit val hensel = new Hensel(r.nextBigInt(_))
    }

    object implicits extends implicits {
        val r = new Random
    }
}
