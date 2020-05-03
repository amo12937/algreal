package amo.geometry.figures

import amo.algreal.field.ConstructibleTrait

trait Figure2D[T] extends Equals {
    implicit val constructible: ConstructibleTrait[T]

    def definingBinomial: Binomial[T]

    def contains(p: Point[T]): Boolean =
        constructible.equiv(
            definingBinomial.valueAt(p),
            constructible.zero
        )

    def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[Figure2D[T]]
    override def equals(rhs: Any): Boolean = rhs match {
        case figure: Figure2D[T] =>
            figure.canEqual(this) &&
            definingBinomial == figure.definingBinomial
        case _ => false
    }
}
