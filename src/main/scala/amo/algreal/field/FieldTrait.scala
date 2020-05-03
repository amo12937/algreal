package amo.algreal.Field

import amo.algreal.EuclideanDomainTrait

trait FieldTrait[T] extends EuclideanDomainTrait[T] {
    def unit(a: T) = if (equiv(a, zero)) one else a
    def gcd(a: T, b: T) =
        if (equiv(a, zero)) b
        else if (equiv(b, zero)) a
        else one

    def divMod(a: T, b: T) = (divide(a, b), zero)
}
