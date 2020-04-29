package amo.geometry.figures

import amo.algreal.{ Closure, GcdDomainTrait, Interval }
import amo.algreal.Field.FieldTrait

import amo.implicits._

class Line[T](
    val a: T, val b: T, val c: T, val p1: Point[T], val p2: Point[T]
)(
    implicit val gcdDomainT: GcdDomainTrait[T],
    ordering: Ordering[T]
) extends LineLike[T] {
    val interval = Interval(Closure.NegativeInfinity, Closure.PositiveInfinity)
}

object Line {
    def apply[T](a: T, b: T, c: T)(
        implicit field: FieldTrait[T],
        ordering: Ordering[T]
    ): Line[T] = {
        val (p1, p2): (Point[T], Point[T]) =
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
        new Line(a, b, c, p1, p2)
    }

    def apply[T](p1: Point[T], p2: Point[T])(
        implicit gcdDomainT: GcdDomainTrait[T],
        ordering: Ordering[T]
    ): Line[T] = {
        val a = gcdDomainT.sub(p2.y, p1.y)
        val b = gcdDomainT.sub(p1.x, p2.x)
        val c = gcdDomainT.negate(gcdDomainT.add(
            gcdDomainT.times(a, p1.x),
            gcdDomainT.times(b, p1.y)
        ))
        new Line(a, b, c, p1, p2)
    }
}
