package amo.geometry.figures

import amo.algreal.Field.FieldTrait

object IntersectionExtension {
    trait implicits {
        implicit class LineSolver[T](
            l1: Line[T]
        )(
            implicit field: FieldTrait[T]
        ) {
            def intersects(l2: Line[T]): Iterator[Point[T]] =
                if (
                    field.equiv(l1.a, l2.a) &&
                    field.equiv(l1.b, l2.b)
                ) Iterator.empty else {
                    val a = Point(l1.a, l2.a)
                    val b = Point(l1.b, l2.b)
                    val c = Point(l1.c, l2.c)
                    Iterator(Point(
                        field.divide(b.det(c), a.det(b)),
                        field.divide(c.det(a), a.det(b))
                    ))
                }
        }

        //implicit class CircleSolver[AlgReal](
        //    c1: Circle[AlgReal]
        //)(
        //    implicit field: FieldTrait[T]
        //)
    }
}
