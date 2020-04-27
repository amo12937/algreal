package amo.geometry.figures

import Binomial._

trait LineLike[T] extends Figure2D[T] {
    val a: T
    val b: T
    val c: T

    val f = a * x + b * y + c

    def definingBinomial: Binomial[T] = f
}
