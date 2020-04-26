package amo.algreal

trait IntegralDomainTrait[T] extends RingTrait[T] {
    def divide(a: T, b: T): T
    def unit(a: T): T
    def normalize(a: T): (T, T) = {
        val u = unit(a)
        (u, divide(a, u))
    }
}
