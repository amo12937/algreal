package amo.AlgReal.Field

import java.lang.ArithmeticException

import amo.AlgReal.{ EqTrait, GcdDomainTrait, RingTrait }

class QuotientField[T](val num: T, val denom: T)(
    implicit gcdDomain: GcdDomainTrait[T]
) extends Equals {
    implicit val nToRingT = gcdDomain.fromInt _

    def + (rhs: QuotientField[T]): QuotientField[T] = QuotientField(
        gcdDomain.add(
            gcdDomain.times(num, rhs.denom),
            gcdDomain.times(denom, rhs.num)
        ),
        gcdDomain.times(denom, rhs.denom)
    )

    def unary_- = QuotientField(gcdDomain.negate(num), denom)

    def - (rhs: QuotientField[T]): QuotientField[T] = this + (-rhs)

    def * (rhs: QuotientField[T]): QuotientField[T] = QuotientField(
        gcdDomain.times(num, rhs.num),
        gcdDomain.times(denom, rhs.denom)
    )

    def inverse: QuotientField[T] = QuotientField(denom, num)

    def / (rhs: QuotientField[T]): QuotientField[T] = this * rhs.inverse

    def pow(n: Int): QuotientField[T] = QuotientField(
        gcdDomain.pow(num, n),
        gcdDomain.pow(denom, n)
    )

    override def toString: String =
        if (gcdDomain.equiv(denom, 1)) num.toString
        else s"${num.toString} / ${denom.toString}"

    def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[QuotientField[T]]
    override def equals(rhs: Any): Boolean = rhs match {
        case r: QuotientField[T] =>
            r.canEqual(this) &&
            gcdDomain.equiv(num, r.num) &&
            gcdDomain.equiv(denom, r.denom)
        case _ => false
    }
}

trait QuotientFieldEqTrait[T] extends EqTrait[QuotientField[T]] {
    implicit val ring: RingTrait[T]
    def equiv(a: QuotientField[T], b: QuotientField[T]): Boolean =
        ring.equiv(ring.times(a.num, b.denom), ring.times(a.denom, b.num))
}

trait QuotientFieldTrait[T] extends FieldTrait[QuotientField[T]] {
    implicit val gcdDomainT: GcdDomainTrait[T]
    implicit val nToRingT: Int => T

    lazy val zero = QuotientField(0, 1)
    lazy val one = QuotientField(1, 1)

    def add(a: QuotientField[T], b: QuotientField[T]) = a + b
    def negate(a: QuotientField[T]) = -a
    def times(a: QuotientField[T], b: QuotientField[T]) = a * b
    def timesN(a: QuotientField[T], n: Int) = a * QuotientField(n, 1)
    def pow(a: QuotientField[T], n: Int) = a pow n

    def divide(a: QuotientField[T], b: QuotientField[T]) = a / b
}

trait QuotientFieldOrdering[T] extends Ordering[QuotientField[T]] {
    implicit val nToRingT: Int => T
    implicit val orderingT: Ordering[T]

    def compare(x: QuotientField[T], y: QuotientField[T]): Int =
        orderingT.compare((x - y).num, 0)
}

trait QuotientFieldCreatorTrait[T] {
    implicit val gcdDomainT: GcdDomainTrait[T]
    implicit val nToRingT: Int => T
    def create(num: T, denom: T): QuotientField[T] = QuotientField(num, denom)
}

object QuotientField {
    def normalize[T](num: T, denom: T)(
        implicit gcdDomain: GcdDomainTrait[T]
    ): (T, T) = {
        implicit val nToRingT = gcdDomain.fromInt _
        if (gcdDomain.equiv(denom, 0)) {
            throw new ArithmeticException("divide by zero")
        }
        if (gcdDomain.equiv(num, 0)) {
            (0, 1)
        } else {
            val g = gcdDomain.gcd(num, denom)
            val (denomU, denomV) = gcdDomain.normalize(gcdDomain.divide(denom, g))
            (
                gcdDomain.divide(gcdDomain.divide(num, g), denomU),
                denomV
            )
        }
    }

    def apply[T](num: T, denom: T)(
        implicit gcdDomain: GcdDomainTrait[T]
    ): QuotientField[T] = {
        val (n, d) = normalize(num, denom)
        new QuotientField(n, d)
    }

    def makeQuotientField[T](
        implicit implicitlyGcdDomainT: GcdDomainTrait[T]
    ) = new QuotientFieldTrait[T]
    with QuotientFieldEqTrait[T]
    with QuotientFieldCreatorTrait[T] {
        val gcdDomainT = implicitlyGcdDomainT
        val ring = implicitlyGcdDomainT
        val nToRingT = implicitlyGcdDomainT.fromInt
    }

    def makeComparableQuotientField[T](
        implicit implicitlyGcdDomainT: GcdDomainTrait[T],
        implicitlyOrderingT: Ordering[T]
    ) = new QuotientFieldTrait[T]
    with QuotientFieldOrdering[T]
    with QuotientFieldCreatorTrait[T] {
        val gcdDomainT = implicitlyGcdDomainT
        val orderingT = implicitlyOrderingT
        val nToRingT = implicitlyGcdDomainT.fromInt
    }
}
