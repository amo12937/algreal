package amo.AlgReal

import java.lang.ArithmeticException
import scala.util.Try

class QuotientField[T](val num: T, val denom: T)(implicit gcdDomain: GcdDomainTrait[T], ordering: Ordering[T]) {
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
}

object QuotientField {
    def normalize[T](num: T, denom: T)(
        implicit gcdDomain: GcdDomainTrait[T],
        ordering: Ordering[T]
    ): (T, T) = {
        val zero = gcdDomain.zero
        val one = gcdDomain.one
        if (ordering.equiv(denom, zero)) {
            throw new ArithmeticException("divide by zero")
        }
        if (ordering.equiv(num, zero)) {
            (zero, one)
        } else if (ordering.lt(denom, zero)) {
            normalize(
                gcdDomain.negate(num),
                gcdDomain.negate(denom)
            )
        } else if (ordering.lt(num, zero)) {
            val (n, d) = normalize(gcdDomain.negate(num), denom)
            (gcdDomain.negate(n), d)
        } else {
            val g = gcdDomain.gcd(num, denom)
            (gcdDomain.divide(num, g), gcdDomain.divide(denom, g))
        }
    }

    def apply[T](num: T, denom: T)(
        implicit gcdDomain: GcdDomainTrait[T],
        ordering: Ordering[T]
    ): QuotientField[T] = {
        val (n, d) = normalize(num, denom)
        new QuotientField(n, d)
    }

    object Implicits {
        import scala.language.implicitConversions
        implicit def sToQuotientField[S, T](s: S)(
            implicit sToT: S => T,
            gcdDomain: GcdDomainTrait[T],
            ordering: Ordering[T]
        ): QuotientField[T] = QuotientField(sToT(s), gcdDomain.one)

        def quotientField[T](
            implicit gcdDomain: GcdDomainTrait[T],
            ordering: Ordering[T]
        ) = new GcdDomainTrait[QuotientField[T]]
        with Ordering[QuotientField[T]] {
            import amo.AlgReal.Ring.Implicits._

            def compare(x: QuotientField[T], y: QuotientField[T]): Int =
                ordering.compare((x - y).num, gcdDomain.zero)

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
