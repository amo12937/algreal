package amo.geometry.figures

import amo.algreal.Field.FieldTrait

case class Point[T](x: T, y: T)(implicit field: FieldTrait[T]) {
    def + (rhs: Point[T]): Point[T] = Point(field.add(x, rhs.x), field.add(y, rhs.y))
    def unary_-(): Point[T] = Point(field.negate(x), field.negate(y))
    def - (rhs: Point[T]): Point[T] = this + (-rhs)

    def innerProduct (rhs: Point[T]): T =
        field.add(
            field.times(x, rhs.x),
            field.times(y, rhs.y)
        )

    def det (rhs: Point[T]): T =
        field.sub(
            field.times(x, rhs.y),
            field.times(y, rhs.x)
        )
}
