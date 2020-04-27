package amo.geometry.figures

import amo.algreal.GcdDomainTrait
import amo.algreal.polynomial.Unipoly
import amo.implicits._

case class Binomial[T](f: Unipoly[Unipoly[T]])(
    implicit gcdDomainT: GcdDomainTrait[T]
) {
    def valueAt(p: Point[T]): T = f.valueAt(Unipoly(p.x)).valueAt(p.y)
}

object Binomial {
    def x[T](
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Binomial[T] = Binomial(Unipoly.ind[Unipoly[T]])
    def y[T](
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Binomial[T] = Binomial(Unipoly(Unipoly.ind[T]))

    trait implicits {
    }
}
