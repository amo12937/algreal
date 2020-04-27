package amo.geometry.figures

import scala.language.implicitConversions

import amo.algreal.{ EuclideanDomainTrait, GcdDomainTrait }
import amo.algreal.polynomial.Unipoly
import amo.implicits._

case class Binomial[T](f: Unipoly[Unipoly[T]])(
    implicit gcdDomainT: GcdDomainTrait[T]
) {
    def valueAt(p: Point[T]): T = f.valueAt(Unipoly(p.x)).valueAt(p.y)

    def + (rhs: Binomial[T]): Binomial[T] = Binomial(f + rhs.f)
    def unary_-(): Binomial[T] = Binomial(-f)
    def - (rhs: Binomial[T]): Binomial[T] = Binomial(f - rhs.f)

    def * (rhs: Binomial[T]): Binomial[T] = Binomial(f * rhs.f)
    def scale(t: T): Binomial[T] = Binomial(f.scale(t))
    def divide (rhs: Binomial[T]): Binomial[T] = Binomial(f divide rhs.f)

    def pow (n: Int): Binomial[T] = Binomial(f pow n)
    def ^ (n: Int): Binomial[T] = pow(n)

    def gcd(rhs: Binomial[T]): Binomial[T] = Binomial(f gcd rhs.f)
}

object Binomial {
    def x[T](
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Binomial[T] = Binomial(Unipoly.ind[Unipoly[T]])
    def y[T](
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Binomial[T] = Binomial(Unipoly(Unipoly.ind[T]))

    def zero[T](
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Binomial[T] = Binomial(Unipoly.zero[Unipoly[T]])

    def one[T](
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Binomial[T] = Binomial(Unipoly.one[Unipoly[T]])

    implicit def apply[T](t: T)(
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Binomial[T] = Binomial(Unipoly(Unipoly(t)))

    trait implicits {
        this: Unipoly.implicits =>

        implicit def makeBinomialEuclideanDomain[T](
            implicit gcdDomainT: GcdDomainTrait[T]
        ) = new EuclideanDomainTrait[Binomial[T]] {
            val gcdDomainU = implicitly[EuclideanDomainTrait[Unipoly[Unipoly[T]]]]
            implicit val nToT = gcdDomainT.fromInt _

            def equiv(a: Binomial[T], b: Binomial[T]) = a == b
            val zero = Binomial.zero
            val one = Binomial.one

            def add(a: Binomial[T], b: Binomial[T]) = a + b
            def negate(a: Binomial[T]) = -a
            def times(a: Binomial[T], b: Binomial[T]) = a * b
            def timesN(a: Binomial[T], n: Int) = a scale n
            def pow(a: Binomial[T], n: Int) = a pow n

            def divide(a: Binomial[T], b: Binomial[T]) = a divide b
            def unit(a: Binomial[T]) = Binomial(gcdDomainU.unit(a.f))
            def gcd(a: Binomial[T], b: Binomial[T]) = a gcd b

            def divMod(a: Binomial[T], b: Binomial[T]) = {
                val (q, r) = gcdDomainU.divMod(a.f, b.f)
                (Binomial(q), Binomial(r))
            }
        }
    }
}
