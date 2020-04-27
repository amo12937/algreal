package amo.geometry.figures

import amo.algreal.GcdDomainTrait

class Line[T](
    val a: T, val b: T, val c: T
)(
    implicit val gcdDomainT: GcdDomainTrait[T]
) extends LineLike[T]

object Line {
    def apply[T](a: T, b: T, c: T)(
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Line[T] = new Line(a, b, c)

    def apply[T](p1: Point[T], p2: Point[T])(
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Line[T] = {
        val a = gcdDomainT.sub(p2.y, p1.y)
        val b = gcdDomainT.sub(p1.x, p2.x)
        val c = gcdDomainT.negate(gcdDomainT.add(
            gcdDomainT.times(a, p1.x),
            gcdDomainT.times(b, p1.y)
        ))
        Line(a, b, c)
    }
}
