package amo.AlgReal

trait RingTrait[T] extends EqTrait[T] {
    val zero: T
    val one: T
    def add(a: T, b: T): T
    def negate(a: T): T
    def sub(a: T, b: T): T = add(a, negate(b))
    def times(a: T, b: T): T
    def timesN(a: T, n: Int): T
    def pow(a: T, n: Int): T

    def fromInt(n: Int): T = timesN(one, n)
}
