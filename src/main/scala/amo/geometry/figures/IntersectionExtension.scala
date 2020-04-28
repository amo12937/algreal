package amo.geometry.figures

import scala.language.higherKinds

import amo.algreal.Field.FieldTrait

trait IntersectionSolver[T, F1[_], F2[_]] {   // T, F1, F2 を与えて、
    def intersects(f1: F1[T], f2: F2[T])(
        implicit ef1: F1[T] <:< Figure2D[T],  // F1[T] が Figure2D[T] の子クラス
        ef2: F2[T] <:< Figure2D[T],           // F2[T] が Figure2D[T] の子クラスだった場合に定義される
        field: FieldTrait[T]
    ): Iterator[Point[T]]
}

object IntersectionExtension {
    trait implicits {
        implicit class HasSolver[T, F1[_]](f1: F1[T])(
            implicit ef1: F1[T] <:< Figure2D[T],
            field: FieldTrait[T]
        ) {
            def intersects[F2[_]](f2: F2[T])(
                implicit ef2: F2[T] <:< Figure2D[T],
                solver: IntersectionSolver[T, F1, F2]
            ): Iterator[Point[T]] = solver.intersects(f1, f2)
        }

        implicit def twoLineSolver[T] = new IntersectionSolver[T, Line, Line] {
            def intersects(f1: Line[T], f2: Line[T])(
                implicit ef1: Line[T] <:< Figure2D[T],
                ef2: Line[T] <:< Figure2D[T],
                field: FieldTrait[T]
            ): Iterator[Point[T]] =
                if (
                    field.equiv(f1.a, f2.a) &&
                    field.equiv(f1.b, f2.b)
                ) Iterator.empty else {
                    val a = Point(f1.a, f2.a)
                    val b = Point(f1.b, f2.b)
                    val c = Point(f1.c, f2.c)
                    Iterator(Point(
                        field.divide(b.det(c), a.det(b)),
                        field.divide(c.det(a), a.det(b))
                    ))
                }
        }
    }
}
