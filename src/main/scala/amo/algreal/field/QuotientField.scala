package amo.algreal.field

import java.lang.ArithmeticException

import amo.algreal.{ EqTrait, GcdDomainTrait, RingTrait }

case class QuotientField[T](num: T, denom: T)(
    implicit gcdDomainT: GcdDomainTrait[T]
) extends Equals {
    implicit val nToRingT = gcdDomainT.fromInt _

    def + (rhs: QuotientField[T]): QuotientField[T] = QuotientField(
        gcdDomainT.add(
            gcdDomainT.times(num, rhs.denom),
            gcdDomainT.times(denom, rhs.num)
        ),
        gcdDomainT.times(denom, rhs.denom)
    )

    def unary_- = QuotientField(gcdDomainT.negate(num), denom)

    def - (rhs: QuotientField[T]): QuotientField[T] = this + (-rhs)

    def * (rhs: QuotientField[T]): QuotientField[T] = QuotientField(
        gcdDomainT.times(num, rhs.num),
        gcdDomainT.times(denom, rhs.denom)
    )

    def inverse: QuotientField[T] = QuotientField(denom, num)

    def / (rhs: QuotientField[T]): QuotientField[T] = this * rhs.inverse

    def pow(n: Int): QuotientField[T] = QuotientField(
        gcdDomainT.pow(num, n),
        gcdDomainT.pow(denom, n)
    )

    override def toString: String =
        if (gcdDomainT.equiv(denom, 1)) num.toString
        else s"${num.toString} / ${denom.toString}"

    def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[QuotientField[T]]
    override def equals(rhs: Any): Boolean = rhs match {
        case r: QuotientField[T] =>
            r.canEqual(this) &&
            gcdDomainT.equiv(num, r.num) &&
            gcdDomainT.equiv(denom, r.denom)
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

    def apply(num: T, denom: T): QuotientField[T] = QuotientField(num, denom)
    def apply(num: T): QuotientField[T] = apply(num, 1)
}

object QuotientField {
    def normalize[T](num: T, denom: T)(
        implicit gcdDomainT: GcdDomainTrait[T]
    ): (T, T) = {
        implicit val nToRingT = gcdDomainT.fromInt _
        if (gcdDomainT.equiv(denom, 0)) {
            throw new ArithmeticException("divide by zero")
        }
        if (gcdDomainT.equiv(num, 0)) {
            (0, 1)
        } else {
            val g = gcdDomainT.gcd(num, denom)
            val (denomU, denomV) = gcdDomainT.normalize(gcdDomainT.divide(denom, g))
            (
                gcdDomainT.divide(gcdDomainT.divide(num, g), denomU),
                denomV
            )
        }
    }

    def apply[T](num: T, denom: T)(
        implicit gcdDomainT: GcdDomainTrait[T]
    ): QuotientField[T] = {
        val (n, d) = normalize(num, denom)
        new QuotientField(n, d)
    }

    def apply[T](num: T)(
        implicit gcdDomainT: GcdDomainTrait[T]
    ): QuotientField[T] = apply(num, gcdDomainT.one)

    def zero[T](implicit gcdDomainT: GcdDomainTrait[T]) =
        apply(gcdDomainT.zero)

    def makeQuotientField[T](
        implicit implicitlyGcdDomainT: GcdDomainTrait[T]
    ) = new QuotientFieldTrait[T]
    with QuotientFieldEqTrait[T] {
        val gcdDomainT = implicitlyGcdDomainT
        val ring = implicitlyGcdDomainT
        val nToRingT = implicitlyGcdDomainT.fromInt
    }

    trait implicits {
        import scala.language.implicitConversions

        implicit def tToQuotientField[T](t: T)(
            implicit gcdDomainT: GcdDomainTrait[T]
        ): QuotientField[T] = QuotientField(t, gcdDomainT.one)

        implicit def autoMakeQuotientField[T](
            implicit implicitlyGcdDomainT: GcdDomainTrait[T]
        ): QuotientFieldTrait[T]
        with QuotientFieldEqTrait[T] = makeQuotientField[T]

        implicit class QuotientFieldMathExtension[T](r: QuotientField[T])(
            implicit gcdDomainT: GcdDomainTrait[T],
            orderingQ: Ordering[QuotientField[T]]
        ) {
            val gcdDomainQ = implicitly[QuotientFieldTrait[T]]
            def floor: T =
                if (orderingQ.lt(r, gcdDomainQ.zero))
                    gcdDomainT.negate(new QuotientFieldMathExtension(-r).ceil)
                else gcdDomainT.divide(r.num, r.denom)

            def ceil: T =
                if (orderingQ.lt(r, gcdDomainQ.zero))
                    gcdDomainT.negate(new QuotientFieldMathExtension(-r).floor)
                else {
                    val q = gcdDomainT.divide(r.num, r.denom)
                    if (gcdDomainT.equiv(r.num, gcdDomainT.times(q, r.denom))) q
                    else gcdDomainT.add(q, gcdDomainT.one)
                }
        }
    }
}
