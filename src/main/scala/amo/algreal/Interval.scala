package amo.algreal

class Interval[T](val left: T, val right: T)(implicit ordering: Ordering[T]) extends Equals {
    def + (rhs: Interval[T])(implicit ring: RingTrait[T]) = Interval(
        ring.add(left, rhs.left),
        ring.add(right, rhs.right)
    )

    def + (rhs: T)(implicit ring: RingTrait[T]) = Interval(
        ring.add(left, rhs),
        ring.add(right, rhs)
    )

    def unary_-(implicit ring: RingTrait[T]) = Interval(
        ring.negate(right), ring.negate(left)
    )

    def - (rhs: Interval[T])(implicit ring: RingTrait[T]) = this + (-rhs)
    def - (rhs: T)(implicit ring: RingTrait[T]) = this + ring.negate(rhs)

    def * (rhs: Interval[T])(implicit ring: RingTrait[T]) = {
        val candidates = Vector(
            ring.times(left, rhs.left),
            ring.times(left, rhs.right),
            ring.times(right, rhs.left),
            ring.times(right, rhs.right)
        )
        Interval(
            candidates.reduce(ordering.min),
            candidates.reduce(ordering.max)
        )
    }

    def * (rhs: T)(implicit ring: RingTrait[T]) = {
        val a = ring.times(left, rhs)
        val b = ring.times(right, rhs)
        Interval(ordering.min(a, b), ordering.max(a, b))
    }

    def inverse(implicit integralDomain: IntegralDomainTrait[T]) =
        if (
            ordering.lt(right, integralDomain.zero) ||
            ordering.lt(integralDomain.zero, left)
        ) Interval(
            integralDomain.divide(integralDomain.one, right),
            integralDomain.divide(integralDomain.one, left)
        )
        else throw new ArithmeticException("divide by 0")

    def abs(implicit ring: RingTrait[T]) =
        if (ordering.lteq(ring.zero, left)) this
        else if (ordering.lteq(right, ring.zero)) -this
        else Interval(
            ring.zero,
            ordering.max(ring.negate(left), right)
        )

    def middle(implicit integralDomain: IntegralDomainTrait[T]): T =
        integralDomain.divide(
            integralDomain.add(left, right),
            integralDomain.timesN(integralDomain.one, 2)
        )

    def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[Interval[T]]
    override def equals(rhs: Any): Boolean = rhs match {
        case r: Interval[T] =>
            r.canEqual(this) &&
            ordering.equiv(left, r.left) &&
            ordering.equiv(right, r.right)
        case _ => false
    }

    override def toString: String = s"[$left, $right]"

    def clamp(x: Closure[T]): T = x match {
        case Closure.NegativeInfinity => left
        case Closure.PositiveInfinity => right
        case Closure.ClosureValue(t) => ordering.min(ordering.max(t, left), right)
    }

    def intersect(rhs: Interval[Closure[T]]): Interval[T] =
        Interval(clamp(rhs.left), clamp(rhs.right))

    def contains(t: T): Boolean =
        ordering.lteq(left, t) && ordering.lteq(t, right)
}

object Interval {
    def apply[T](left: T, right: T)(
        implicit ordering: Ordering[T],
    ) =
        if (ordering.gt(left, right)) new Interval(right, left)
        else new Interval(left, right)
}
