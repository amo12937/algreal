package amo.algreal.Field

trait AlgebraicClosureTrait[T] extends ConstructibleTrait[T] {
    def nthRoot(a: T, n: Int): T
    def sqrt(a: T): T = nthRoot(a, 2)
}
