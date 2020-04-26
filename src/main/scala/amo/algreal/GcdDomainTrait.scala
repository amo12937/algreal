package amo.algreal

trait GcdDomainTrait[T] extends IntegralDomainTrait[T] {
    def gcd(a: T, b: T): T
    def content(xs: Vector[T]): T = xs.foldLeft(zero)(gcd)
}
