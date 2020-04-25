package amo.AlgReal

import amo.implicits._
import amo.AlgReal.Field.QuotientField

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

