package amo.AlgReal

import java.lang.ArithmeticException

class QuotientField[T](val num: T, val denom: T)(
    implicit gcdDomain: GcdDomainTrait[T],
    nToRing: Int => T
) extends Equals {
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

trait QuotientFieldGcdDomainTrait[T] extends GcdDomainTrait[QuotientField[T]] {
    implicit val nToRing: Int => QuotientField[T]

    lazy val zero = 0
    lazy val one = 1

    def add(a: QuotientField[T], b: QuotientField[T]) = a + b
    def negate(a: QuotientField[T]) = -a
    def times(a: QuotientField[T], b: QuotientField[T]) = a * b
    def timesN(a: QuotientField[T], n: Int) = a * n
    def pow(a: QuotientField[T], n: Int) = a pow n

    def divide(a: QuotientField[T], b: QuotientField[T]) = a / b
    def unit(a: QuotientField[T]) = if (equiv(a, 0)) 1 else a

    def gcd(a: QuotientField[T], b: QuotientField[T]) =
        if (equiv(a, 0) && equiv(b, 0)) 0 else 1

    def content(xs: Vector[QuotientField[T]]) = xs.reduceLeft(gcd)
}

trait QuotientFieldOrdering[T] extends Ordering[QuotientField[T]] {
    implicit val nToRingT: Int => T
    implicit val orderingT: Ordering[T]

    def compare(x: QuotientField[T], y: QuotientField[T]): Int =
        orderingT.compare((x - y).num, 0)
}

object QuotientField {
    def normalize[T](num: T, denom: T)(
        implicit gcdDomain: GcdDomainTrait[T],
        nToRing: Int => T
    ): (T, T) = {
        if (gcdDomain.equiv(denom, 0)) {
            throw new ArithmeticException("divide by zero")
        }
        if (gcdDomain.equiv(num, 0)) {
            (0, 1)
        } else {
            val (numU, numV) = gcdDomain.normalize(num)
            val (denomU, denomV) = gcdDomain.normalize(denom)
            val g = gcdDomain.gcd(numV, denomV)
            (
                gcdDomain.divide(gcdDomain.divide(num, g), denomU),
                gcdDomain.divide(denomV, g)
            )
        }
    }

    def apply[T](num: T, denom: T)(
        implicit gcdDomain: GcdDomainTrait[T],
        nToRing: Int => T
    ): QuotientField[T] = {
        val (n, d) = normalize(num, denom)
        new QuotientField(n, d)
    }

    trait implicits {
        import scala.language.implicitConversions

        implicit def sToQuotientField[S, T](s: S)(
            implicit gcdDomain: GcdDomainTrait[T],
            nToRing: Int => T,
            sToT: S => T
        ): QuotientField[T] = QuotientField(sToT(s), 1)

        def quotientField[T](
            implicit gcdDomainT: GcdDomainTrait[T],
            nToRingT: Int => T
        ) = new QuotientFieldGcdDomainTrait[T] with QuotientFieldEqTrait[T] {
            implicit val nToRing = sToQuotientField(_)(gcdDomainT, nToRingT, nToRingT)
            implicit val ring = gcdDomainT
        }

        def comparableQuotientField[T](
            implicit gcdDomainT: GcdDomainTrait[T],
            orderingTT: Ordering[T],
            nToRingTT: Int => T
        ) = new QuotientFieldGcdDomainTrait[T] with QuotientFieldOrdering[T] {
            implicit val nToRing = sToQuotientField(_)(gcdDomainT, nToRingT, nToRingT)
            implicit val orderingT = orderingTT
            implicit val nToRingT = nToRingTT
        }
    }
}
