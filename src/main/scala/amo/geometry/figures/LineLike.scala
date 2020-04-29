package amo.geometry.figures

import amo.algreal.{ Closure, Interval }
import amo.algreal.Field.ConstructibleTrait
import amo.algreal.Closure.implicits._
import Binomial._

trait LineLike[T] extends Figure2D[T] {
    val a: T
    val b: T
    val c: T

    val p1: Point[T]
    val p2: Point[T]
    lazy val direction = p2 - p1
    lazy val eDirection = direction.scalarDiv(direction.norm)

    val interval: Interval[Closure[T]]

    lazy val f = a * x + b * y + c

    def definingBinomial: Binomial[T] = f

    def innerProductIsInInterval(p: Point[T]): Boolean = {
        val ip = Closure(eDirection.innerProduct(p - p1))
        interval.contains(ip)
    }

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
    def normalize[T](a: T, b: T, c: T)(
        implicit constructible: ConstructibleTrait[T]
    ): (T, T, T) = 
        if (!constructible.equiv(a, constructible.zero)) (
            constructible.one,
            constructible.divide(b, a),
            constructible.divide(c, a)
        ) else if (!constructible.equiv(b, constructible.zero)) (
            constructible.divide(a, b),
            constructible.one,
            constructible.divide(c, b)
        ) else throw new ArithmeticException(s"ilegal line ($a, $b, $c)")

    def abcToPoints[T](a: T, b: T, c: T)(
        implicit constructible: ConstructibleTrait[T]
    ): (Point[T], Point[T]) = 
        if (constructible.equiv(b, constructible.zero)) {
            // ax + c = 0
            val x = constructible.divide(constructible.negate(c), a)
            (Point(x, constructible.zero), Point(x, constructible.one))
        } else {
            // ax + by + c = 0
            val y0 = constructible.divide(constructible.negate(c), b)
            val y1 = constructible.divide(
                constructible.negate(constructible.add(a, c)), b
            )
            (Point(constructible.zero, y0), Point(constructible.one, y1))
        }

    def pointsToABC[T](p1: Point[T], p2: Point[T])(
        implicit constructible: ConstructibleTrait[T]
    ): (T, T, T) = {
        val a = constructible.sub(p2.y, p1.y)
        val b = constructible.sub(p1.x, p2.x)
        val c = constructible.negate(constructible.add(
            constructible.times(a, p1.x),
            constructible.times(b, p1.y)
        ))
        normalize(a, b, c)
    }

    def apply[T](_a: T, _b: T, _c:T, _interval: Interval[Closure[T]])(
        implicit _constructible: ConstructibleTrait[T]
    ): LineLike[T] = new LineLike[T] {
        val constructible = _constructible
        val (a, b, c) = normalize(_a, _b, _c)
        val (p1, p2) = abcToPoints(a, b, c)
        val interval = _interval
    }

    def apply[T](_p1: Point[T], _p2: Point[T], _interval: Interval[Closure[T]])(
        implicit _constructible: ConstructibleTrait[T]
    ): LineLike[T] = new LineLike[T] {
        val constructible = _constructible
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
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): LineLike[T] = LineLike(a, b, c, interval)

    def apply[T](p1: Point[T], p2: Point[T])(
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): LineLike[T] = LineLike(p1, p2, interval)
}

object HalfLine {
    def interval[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Interval[Closure[T]] =
        Interval(Closure(constructible.zero), Closure.PositiveInfinity)

    def apply[T](p1: Point[T], p2: Point[T])(
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): LineLike[T] = LineLike(p1, p2, interval)
}

object LineSegment {
    def interval[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Interval[Closure[T]] =
        Interval(Closure(constructible.zero), Closure(constructible.one))

    def apply[T](p1: Point[T], p2: Point[T])(
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): LineLike[T] = LineLike(p1, p2, interval)
}
