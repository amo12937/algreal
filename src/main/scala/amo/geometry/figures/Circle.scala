package amo.geometry.figures

import amo.algreal.Field.ConstructibleTrait
import Binomial._

case class Circle[T](
    val p: Point[T],
    val r: T
)(
    implicit val constructible: ConstructibleTrait[T]
) extends Figure2D[T] {
    val f = ((x - p.x)^2) + ((y - p.y)^2) - (r^2)

    def definingBinomial: Binomial[T] = f
}
