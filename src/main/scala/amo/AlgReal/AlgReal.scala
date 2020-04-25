package amo.AlgReal

import scala.util.Random

import amo.implicits._
import amo.AlgReal.Field.QuotientField
import amo.AlgReal.factors.Hensel

sealed trait AlgReal {
    def definingPolynomial: Unipoly[BigInt]
    def isolatingInterval: Interval[QuotientField[BigInt]]

    def intervals: Iterator[Interval[QuotientField[BigInt]]]

    def compare(rhs: AlgReal): Int
}

case class Rat(val r: QuotientField[BigInt]) extends AlgReal {
    val f = Unipoly(-r.num, r.denom)
    val i = Interval(r - 1, r + 1)

    def definingPolynomial = f
    def isolatingInterval = i
    def intervals = {
        val iv = Interval(r, r)
        Iterator.continually(iv)
    }

    def compare(rhs: AlgReal) = rhs match {
        case Rat(rhsR) => rational.compare(r, rhsR)
        case AlgRealPoly(rhsF, rhsS, rhsI) => {
            if (rational.lteq(r, rhsI.left)) -1
            else if (rational.lteq(rhsI.right, r)) 1
            else if (rhsF.countRealRootsBetween(r, rhsI.right) == 1) -1
            else if (rhsF.valueAt(r) == 0) 0
            else 1
        }
    }
}

case class AlgRealPoly(
    val f: Unipoly[BigInt],
    val s: Int,
    val i: Interval[QuotientField[BigInt]]
) extends AlgReal {
    def definingPolynomial = f
    def isolatingInterval = i

    def intervals = f.intervalsWithSign(s, i)

    def compare(rhs: AlgReal) = rhs match {
        case x: Rat => -x.compare(this)
        case AlgRealPoly(_, _, rhsI) if rational.lteq(i.right, rhsI.left) => -1
        case AlgRealPoly(_, _, rhsI) if rational.lteq(rhsI.right, i.left) => 1
        case AlgRealPoly(rhsF, _, rhsI) if (f.gcd(rhsF).countRealRootsBetween(
            rational.max(i.left, rhsI.left),
            rational.min(i.right, rhsI.right)
        ) == 1) => 0
        case _ => this.intervals.zip(rhs.intervals).flatMap({ case (ivL, ivR) =>
            if (rational.lteq(ivL.right, ivR.left)) Some(-1)
            else if (rational.lteq(ivR.right, ivL.left)) Some(1)
            else None
        }).next
    }
}

object AlgReal {
    val r = new Random
    val hensel: Hensel = new Hensel(r.nextBigInt(_))

    def mkAlgReal(
        f: Unipoly[BigInt],
        iv: Interval[QuotientField[BigInt]]
    ): AlgReal = {
        if (iv.left == iv.right && f.valueAt(iv.left) == 0) Rat(iv.left)
        else if (iv.contains(0) && f.valueAt(0) == 0) Rat(0)
        else if (f.degreeInt == 1) f.cs match {
            case Vector(a, b) => Rat(rational.create(-a, b))
        }
        else {
            val s = f.signAt(iv.right) match {
                case 0 => f.diff.signAt(iv.right)
                case k => k
            }
            f.intervalsWithSign(s, iv)
                .find(iv2 => !iv2.contains(0))
                .map(AlgRealPoly(f, s, _))
                .getOrElse(Rat(0))
        }
    }

    def bisect(
        f: Unipoly[BigInt],
        seq: Vector[Unipoly[BigInt]],
        iv: Interval[QuotientField[BigInt]],
        i: Int,
        j: Int
    ): Iterator[AlgReal] =
        if (i <= j) Iterator.empty
        else if (i == j + 1) Iterator(mkAlgReal(f, iv))
        else {
            val c = iv.middle
            val k = StrumExtension.varianceAt(c, seq)
            bisect(f, seq, Interval(iv.left, c), i, k) ++ bisect(f, seq, Interval(c, iv.right), k, j)
        }

    def realRootsBetween(
        f: Unipoly[BigInt],
        lb: Closure[QuotientField[BigInt]],
        ub: Closure[QuotientField[BigInt]]
    ): Iterator[AlgReal] = if (f.degreeInt <= 0) Iterator.empty else {
        val f2 = f.squareFree
        val seq = f2.negativePRS(f2.diff).toVector
        val b = f2.rootBound
        val boundIv = Interval(-b, b).intersect(Interval(lb, ub))

        bisect(
            f, seq, boundIv,
            StrumExtension.varianceAt(boundIv.left, seq),
            StrumExtension.varianceAt(boundIv.right, seq)
        )
    }

    def realRoots(
        f: Unipoly[BigInt]
    ): Iterator[AlgReal] =
        if (f.degreeInt <= 0) Iterator.empty else for {
            f2 <- Iterator(f.squareFree)
            g <- hensel.factor(f2)
            x <- realRootsBetween(
                g, Closure.NegativeInfinity, Closure.PositiveInfinity
            )
        } yield x
}
