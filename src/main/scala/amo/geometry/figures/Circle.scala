package amo.geometry.figures

import amo.algreal.GcdDomainTrait
import Binomial._

class Circle[T](
    p: Point[T],
    r: T
)(
    implicit val gcdDomainT: GcdDomainTrait[T]
) extends Figure2D[T] {
    val f = ((x - p.x)^2) + ((y - p.y)^2) - (r^2)

    def definingBinomial: Binomial[T] = f
}
