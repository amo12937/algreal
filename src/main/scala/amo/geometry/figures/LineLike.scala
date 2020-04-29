package amo.geometry.figures

import amo.algreal.{ Closure, Interval, GcdDomainTrait }
import amo.algreal.Field.FieldTrait
import amo.algreal.Closure.implicits._
import Binomial._

trait LineLike[T] extends Figure2D[T] {
    val a: T
    val b: T
    val c: T

    val p1: Point[T]
    val p2: Point[T]

    val interval: Interval[Closure[T]]

    lazy val f = a * x + b * y + c

    def definingBinomial: Binomial[T] = f

    override def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[LineLike[T]]
    override def equals(rhs: Any): Boolean = rhs match {
        case l: LineLike[T] =>
            l.canEqual(this) &&
            definingBinomial == l.definingBinomial &&
            interval == l.interval
        case _ => false
    }
}

object LineLike {
    def abcToPoints[T](a: T, b: T, c: T)(
        implicit field: FieldTrait[T]
    ): (Point[T], Point[T]) = 
        if (field.equiv(b, field.zero)) {
            // ax + c = 0
            val x = field.divide(field.negate(c), a)
            (Point(x, field.zero), Point(x, field.one))
        } else {
            // ax + by + c = 0
            val y0 = field.divide(field.negate(c), b)
            val y1 = field.divide(
                field.negate(field.add(a, c)), b
            )
            (Point(field.zero, y0), Point(field.one, y1))
        }

    def pointsToABC[T](p1: Point[T], p2: Point[T])(
        implicit gcdDomainT: GcdDomainTrait[T]
    ): (T, T, T) = {
        val a = gcdDomainT.sub(p2.y, p1.y)
        val b = gcdDomainT.sub(p1.x, p2.x)
        val c = gcdDomainT.negate(gcdDomainT.add(
            gcdDomainT.times(a, p1.x),
            gcdDomainT.times(b, p1.y)
        ))
        (a, b, c)
    }

    def apply[T](_a: T, _b: T, _c:T, _interval: Interval[Closure[T]])(
        implicit field: FieldTrait[T]
    ): LineLike[T] = new LineLike[T] {
        val gcdDomainT = field
        val a = _a
        val b = _b
        val c = _c
        val (p1, p2) = abcToPoints(a, b, c)
        val interval = _interval
    }

    def apply[T](_p1: Point[T], _p2: Point[T], _interval: Interval[Closure[T]])(
        implicit _gcdDomainT: GcdDomainTrait[T]
    ): LineLike[T] = new LineLike[T] {
        val gcdDomainT = _gcdDomainT
        val p1 = _p1
        val p2 = _p2
        val (a, b, c) = pointsToABC(p1, p2)
        val interval = _interval
    }
}

object Line {
    def interval[T](implicit ordering: Ordering[T]): Interval[Closure[T]] =
        Interval(Closure.NegativeInfinity, Closure.PositiveInfinity)

    def apply[T](a: T, b: T, c: T)(
        implicit field: FieldTrait[T],
        ordering: Ordering[T]
    ): LineLike[T] = LineLike(a, b, c, interval)

    def apply[T](p1: Point[T], p2: Point[T])(
        implicit gcdDomainT: GcdDomainTrait[T],
        ordering: Ordering[T]
    ): LineLike[T] = LineLike(p1, p2, interval)
}
