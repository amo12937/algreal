package amo.geometry.figures

import amo.algreal.GcdDomainTrait

trait Figure2D[T] extends Equals {
    implicit val gcdDomainT: GcdDomainTrait[T]

    def definingBinomial: Binomial[T]

    def contains(p: Point[T]): Boolean =
        gcdDomainT.equiv(
            definingBinomial.valueAt(p),
            gcdDomainT.zero
        )

    def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[Figure2D[T]]
    override def equals(rhs: Any): Boolean = rhs match {
        case figure: Figure2D[T] =>
            definingBinomial == figure.definingBinomial
        case _ => false
    }
}
