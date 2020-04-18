package amo.AlgReal

trait IntegralDomainTrait[T] extends RingTrait[T] {
    this: RingTrait[T] =>
    def divide(a: T, b: T): T
}
