package amo.AlgReal

import java.lang.ArithmeticException
import scala.util.Try

class QuotientField[T](val num: T, val denom: T)(implicit gcdDomain: GcdDomainTrait[T], ordering: Ordering[T], nToRing: Int => T) {
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
        if (ordering.equiv(denom, 1)) num.toString
        else s"${num.toString} / ${denom.toString}"
}

object QuotientField {
    def normalize[T](num: T, denom: T)(
        implicit gcdDomain: GcdDomainTrait[T],
        ordering: Ordering[T],
        nToRing: Int => T
    ): (T, T) = {
        if (ordering.equiv(denom, 0)) {
            throw new ArithmeticException("divide by zero")
        }
        if (ordering.equiv(num, 0)) {
            (0, 1)
        } else if (ordering.lt(denom, 0)) {
            normalize(
                gcdDomain.negate(num),
                gcdDomain.negate(denom)
            )
        } else if (ordering.lt(num, 0)) {
            val (n, d) = normalize(gcdDomain.negate(num), denom)
            (gcdDomain.negate(n), d)
        } else {
            val g = gcdDomain.gcd(num, denom)
            (gcdDomain.divide(num, g), gcdDomain.divide(denom, g))
        }
    }

    def apply[T](num: T, denom: T)(
        implicit gcdDomain: GcdDomainTrait[T],
        ordering: Ordering[T],
        nToRing: Int => T
    ): QuotientField[T] = {
        val (n, d) = normalize(num, denom)
        new QuotientField(n, d)
    }

    trait implicits {
        import scala.language.implicitConversions

        implicit def sToQuotientField[S, T](s: S)(
            implicit gcdDomain: GcdDomainTrait[T],
            ordering: Ordering[T],
            nToRing: Int => T,
            sToT: S => T
        ): QuotientField[T] = QuotientField(sToT(s), 1)

        def quotientField[T](
            implicit gcdDomain: GcdDomainTrait[T],
            ordering: Ordering[T],
            nToRing: Int => T
        ) = new GcdDomainTrait[QuotientField[T]]
        with Ordering[QuotientField[T]] {
            def compare(x: QuotientField[T], y: QuotientField[T]): Int =
                ordering.compare((x - y).num, 0)

            val zero = 0
            val one = 1

            def add(a: QuotientField[T], b: QuotientField[T]) = a + b
            def negate(a: QuotientField[T]) = -a
            def times(a: QuotientField[T], b: QuotientField[T]) = a * b
            def timesN(a: QuotientField[T], n: Int) = a * n
            def pow(a: QuotientField[T], n: Int) = a pow n

            def divide(a: QuotientField[T], b: QuotientField[T]) = a / b

            def gcd(a: QuotientField[T], b: QuotientField[T]) =
                if (equiv(a, 0) && equiv(b, 0)) 0 else 1

            def content(xs: Seq[QuotientField[T]]) = xs.reduceLeft(gcd)
        }
    }
}
