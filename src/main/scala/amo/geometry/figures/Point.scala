package amo.geometry.figures

import amo.algreal.field.{ ConstructibleTrait, FieldTrait }

case class Point[T](x: T, y: T)(implicit field: FieldTrait[T]) {
    def + (rhs: Point[T]): Point[T] = Point(field.add(x, rhs.x), field.add(y, rhs.y))
    def unary_-(): Point[T] = Point(field.negate(x), field.negate(y))
    def - (rhs: Point[T]): Point[T] = this + (-rhs)

    def scalar(t: T): Point[T] = Point(field.times(x, t), field.times(y, t))
    def scalarDiv(t: T): Point[T] = scalar(field.divide(field.one, t))

    def innerProduct(rhs: Point[T]): T =
        field.add(
            field.times(x, rhs.x),
            field.times(y, rhs.y)
        )

    def det(rhs: Point[T]): T =
        field.sub(
            field.times(x, rhs.y),
            field.times(y, rhs.x)
        )

    def squareNorm(implicit constructible: ConstructibleTrait[T]): T =
        constructible.add(
            constructible.pow(x, 2),
            constructible.pow(y, 2),
        )

    def norm(implicit constructible: ConstructibleTrait[T]): T =
        constructible.sqrt(squareNorm)

    def dist(rhs: Point[T])(implicit constructible: ConstructibleTrait[T]): T =
        (this - rhs).norm
}
