package amo.AlgReal

trait GcdDomainTrait[T] extends IntegralDomainTrait[T] {
    def gcd(a: T, b: T): T
    def content(xs: Vector[T]): T
}
