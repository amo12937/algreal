package amo.algreal.Field

trait ConstructibleTrait[T] extends FieldTrait[T] {
    def sqrt(a: T): T

    def realRoots(a: T, b: T, c: T)(implicit ordering: Ordering[T]): Iterator[T] = 
        if (!equiv(a, zero)) {
            val D = sub(pow(b, 2), times(times(fromInt(4), a), c))
            lazy val a2 = timesN(a, 2)
            lazy val nb = negate(b)
            if (ordering.lt(D, zero)) Iterator.empty
            else if (ordering.equiv(D, zero)) Iterator(divide(nb, a2))
            else Iterator(
                divide(add(nb, sqrt(D)), a2),
                divide(sub(nb, sqrt(D)), a2)
            )
        } else if (!equiv(b, zero)) Iterator(divide(negate(c), b))
        else Iterator.empty
}
