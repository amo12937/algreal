package amo.AlgReal

class Interval[T](val left: T, val right: T)(implicit ordering: Ordering[T]) extends Equals {
    def + (rhs: Interval[T])(implicit ring: RingTrait[T]) = Interval(
        ring.add(left, rhs.left),
        ring.add(right, rhs.right)
    )

    def unary_-(implicit ring: RingTrait[T]) = Interval(
        ring.negate(right), ring.negate(left)
    )

    def - (rhs: Interval[T])(implicit ring: RingTrait[T]) = this + (-rhs)

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
}

object Interval {
    def apply[T](left: T, right: T)(
        implicit ordering: Ordering[T],
    ) =
        if (ordering.gt(left, right)) new Interval(right, left)
        else new Interval(left, right)
}
