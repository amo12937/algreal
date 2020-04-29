package amo.geometry.figures

import amo.algreal.{ Closure, Interval }
import Binomial._

trait LineLike[T] extends Figure2D[T] {
    val a: T
    val b: T
    val c: T

    val p1: Point[T]
    val p2: Point[T]

    val interval: Interval[Closure[T]]

    val f = a * x + b * y + c

    def definingBinomial: Binomial[T] = f

    override def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[LineLike[T]]
    override def equals(rhs: Any): Boolean = rhs match {
        case l: Line[T] =>
            l.canEqual(this) &&
            definingBinomial == l.definingBinomial &&
            interval == l.interval
        case _ => false
    }
}
