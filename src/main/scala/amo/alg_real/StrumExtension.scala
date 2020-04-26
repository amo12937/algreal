package amo.AlgReal

import amo.AlgReal.Field.QuotientField
import amo.implicits._

object StrumExtension {
    def variance(signs: Vector[Int]): Int =
        signs.filter(_ != 0).sliding(2).filter({
            case Vector(a, b) => a * b < 0
            case _ => false
        }).length

    def varianceAt[T](
        q: QuotientField[T], fs: Vector[Unipoly[T]]
    )(
        implicit tToF: T => QuotientField[T],
        gcdDomainF: GcdDomainTrait[QuotientField[T]],
        orderingF: Ordering[QuotientField[T]]
    ): Int = variance(fs.map(_.signAt(q)))

    trait implicits {
        implicit class UnipolyStrumExtension[T](unipoly: Unipoly[T])(
            implicit gcdDomainT: GcdDomainTrait[T],
            orderingT: Ordering[T]
        ) {
            implicit val nToRingT = gcdDomainT.fromInt _
            implicit val comparableGcdDomainF = QuotientField.makeComparableQuotientField[T]
            implicit val nToRingF = comparableGcdDomainF.fromInt _

            def intervalsWithSign(
                s: Int,
                interval: Interval[QuotientField[T]]
            ): Iterator[Interval[QuotientField[T]]] =
                Iterator.iterate(
                    (interval, false)
                )({ case (iv, isRat) => if (isRat) (iv, isRat) else {
                    val middle = iv.middle
                    val v = comparableGcdDomainF.timesN(unipoly.valueAt(middle), s)
                    comparableGcdDomainF.compare(v, comparableGcdDomainF.zero) match {
                        case 0 => (Interval(middle, middle), true)
                        case -1 => (Interval(middle, iv.right), false)
                        case _ => (Interval(iv.left, middle), false)
                    }
                }}).map(_._1)

            def negativePRS(rhs: Unipoly[T]): Iterator[Unipoly[T]] = {
                Iterator(unipoly) ++ unipoly.subresultantPRS(rhs).scanLeft(
                    (unipoly, rhs, 1, 1)
                )({case ((f, g, s, t), (b, x)) =>
                    val lsign = gcdDomainT.signum(g.leadingCoefficient)
                    val lsignpow =
                        if (lsign >= 0) lsign
                        else 1 - 2 * ((f.degreeInt - g.degreeInt + 1) % 2)
                    val a = gcdDomainT.signum(gcdDomainT.timesN(b, lsignpow * s))
                    (g, x, t, -a)
                }).map({case (_, g, _, t) => g.scale(t)})
                    .takeWhile(f => !f.isZero)
            }

            def countRealRootsBetween(
                a: QuotientField[T], b: QuotientField[T]
            ): Int = {
                val fs = negativePRS(unipoly.diff).toVector
                varianceAt(a, fs) - varianceAt(b, fs)
            }

            def rootBound: QuotientField[T] = unipoly.cs match {
                case heads :+ lc => heads
                    .map(comparableGcdDomainF.divide(_, lc))
                    .map(comparableGcdDomainF.abs)
                    .foldLeft(comparableGcdDomainF.zero)(comparableGcdDomainF.max) + 1
                case _ => 0
            }
        }
    }
}
