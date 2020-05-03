package amo.algreal.field

trait FiniteFieldTrait[T] extends FieldTrait[T] {
    def order(t: T): BigInt
    def characteristic(t: T): BigInt
}
