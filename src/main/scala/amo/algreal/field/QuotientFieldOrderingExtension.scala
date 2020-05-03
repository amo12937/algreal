package amo.algreal.field

import amo.algreal.{ GcdDomainTrait }

object QuotientFieldOrderingExtension {
    trait implicits {
        implicit def quotientFieldOrdering[T](
            implicit gcdDomainT: GcdDomainTrait[T],
            orderingT: Ordering[T]
        ): Ordering[QuotientField[T]] = new Ordering[QuotientField[T]] {
            def compare(x: QuotientField[T], y: QuotientField[T]): Int =
                orderingT.compare((x - y).num, gcdDomainT.zero)
        }

        implicit class QuotientFieldOrderedExtension[T](
            q: QuotientField[T]
        )(
            implicit orderingQ: Ordering[QuotientField[T]]
        ) extends Ordered[QuotientField[T]] {
            def compare(rhs: QuotientField[T]): Int = orderingQ.compare(q, rhs)
        }
    }
}
