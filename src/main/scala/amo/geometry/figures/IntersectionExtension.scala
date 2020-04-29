package amo.geometry.figures

import scala.language.higherKinds

import amo.algreal.Field.{ ConstructibleTrait }

trait IntersectionSolver[T, Fig1[_], Fig2[_]] {   // T, Fig1, Fig2 を与えて、
    def intersects(f1: Fig1[T], f2: Fig2[T])(
        implicit ef1: Fig1[T] <:< Figure2D[T],  // Fig1[T] が Figure2D[T] の子クラス
        ef2: Fig2[T] <:< Figure2D[T],           // Fig2[T] が Figure2D[T] の子クラスだった場合に定義される
        constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Iterator[Point[T]]
}

object IntersectionExtension {
    trait implicits {
        implicit class HasSolver[T, Fig1[_]](f1: Fig1[T])(
            implicit ef1: Fig1[T] <:< Figure2D[T]
        ) {
            def intersects[Fig2[_], Field[_]](f2: Fig2[T])(
                implicit ef2: Fig2[T] <:< Figure2D[T],
                constructible: ConstructibleTrait[T],
                ordering: Ordering[T],
                solver: IntersectionSolver[T, Fig1, Fig2]
            ): Iterator[Point[T]] = solver.intersects(f1, f2)
        }

        implicit def twoLineSolver[T] = new IntersectionSolver[T, LineLike, LineLike] {
            def intersects(f1: LineLike[T], f2: LineLike[T])(
                implicit ef1: LineLike[T] <:< Figure2D[T],
                ef2: LineLike[T] <:< Figure2D[T],
                constructible: ConstructibleTrait[T],
                ordering: Ordering[T]
            ): Iterator[Point[T]] =
                if (
                    constructible.equiv(f1.a, f2.a) &&
                    constructible.equiv(f1.b, f2.b)
                ) Iterator.empty else {
                    val a = Point(f1.a, f2.a)
                    val b = Point(f1.b, f2.b)
                    val c = Point(f1.c, f2.c)
                    Iterator(Point(
                        constructible.divide(b.det(c), a.det(b)),
                        constructible.divide(c.det(a), a.det(b))
                    )).filter(p =>
                        f1.innerProductIsInInterval(p) &&
                        f2.innerProductIsInInterval(p)
                    )
                }
        }

        implicit def LineCircleSolver[T] = new IntersectionSolver[
            T, LineLike, Circle
        ] {
            def intersects(f1: LineLike[T], f2: Circle[T])(
                implicit ef1: LineLike[T] <:< Figure2D[T],
                ef2: Circle[T] <:< Figure2D[T],
                constructible: ConstructibleTrait[T],
                ordering: Ordering[T]
            ) = (
                if (constructible.equiv(f1.b, constructible.zero)) {
                    val x = constructible.divide(constructible.negate(f1.c), f1.a)
                    constructible.realRoots(
                        constructible.one,
                        constructible.zero,
                        constructible.sub(
                            constructible.pow(constructible.sub(x, f2.p.x), 2),
                            constructible.pow(f2.r, 2)
                        ) // y^2 + (x - f2.p.x)^2 - (f2.r)^2 = 0
                    ).map(y => Point(x, constructible.add(y, f2.p.y)))
                } else {
                    /*
                     * aX + bY + c = 0
                     * (X-d)^2 + (Y-e)^2 = f^2
                     *
                     * S = X - d
                     * T = Y - e
                     * C = ad + be + c
                     */
                    val C = constructible.add(constructible.add(
                        constructible.times(f1.a, f2.p.x),
                        constructible.times(f1.b, f2.p.y)
                    ), f1.c)

                    /*
                     * aS + bT + C = 0
                     * S^2 + T^2 = f^2
                     * b^2S^2 + b^2T^2 = b^2f^2
                     *
                     * b^2S^2 + (aS + C)^2 = b^2f^2
                     * (a^2 + b^2)S^2 + 2aCS + C^2 - b^2f^2 = 0
                     *
                     */
                    val a = constructible.add(constructible.pow(f1.a, 2), constructible.pow(f1.b, 2))
                    val b = constructible.timesN(constructible.times(f1.a, C), 2)
                    val c = constructible.sub(
                        constructible.pow(C, 2),
                        constructible.pow(constructible.times(f1.b, f2.r), 2)
                    )
                    constructible.realRoots(a, b, c).map(S => {
                        val T = constructible.divide(constructible.negate(constructible.add(constructible.times(f1.a, S), C)), f1.b)
                        Point(constructible.add(S, f2.p.x), constructible.add(T, f2.p.y))
                    })
                }
            ).filter(p => f1.innerProductIsInInterval(p))
        }

        implicit def CircleLineSolver[T] = new IntersectionSolver[
            T, Circle, LineLike
        ] {
            def intersects(f1: Circle[T], f2: LineLike[T])(
                implicit ef1: Circle[T] <:< Figure2D[T],
                ef2: LineLike[T] <:< Figure2D[T],
                constructible: ConstructibleTrait[T],
                ordering: Ordering[T]
            ) = LineCircleSolver.intersects(f2, f1)(ef2, ef1, constructible, ordering)
        }

        implicit def twoCircleSolver[T] = new IntersectionSolver[
            T, Circle, Circle
        ] {
            def intersects(f1: Circle[T], f2: Circle[T])(
                implicit ef1: Circle[T] <:< Figure2D[T],
                ef2: Circle[T] <:< Figure2D[T],
                constructible: ConstructibleTrait[T],
                ordering: Ordering[T]
            ) = {
                val d = f1.p.dist(f2.p)
                val maxR = ordering.max(f1.r, f2.r)
                val minR = ordering.min(f1.r, f2.r)
                val lb = constructible.sub(maxR, minR)
                val ub = constructible.add(maxR, minR)
                if (ordering.lt(d, lb) || ordering.lt(ub, d)) Iterator.empty
                else if (constructible.equiv(d, lb)) Iterator(
                    (f1.p.scalar(f2.r) - f2.p.scalar(f1.r)).scalarDiv(constructible.sub(f2.r, f1.r))
                )
                else if (constructible.equiv(d, ub)) Iterator(
                    (f1.p.scalar(f2.r) + f2.p.scalar(f1.r)).scalarDiv(constructible.add(f2.r, f1.r))
                ) else {
                    //   (x - a)^2 + (y - b)^2 = r^2
                    // - (x - c)^2 - (y - d)^2 = -s^2
                    // (2x - a - c)(-a + c) + (2y - b - d)(-b + d) = r^2 - s^2
                    // 2x(-a + c) + (a^2 - c^2) + 2y(-b + d) + (b^2 - d^2) = r^2 - s^2
                    // 2(a-c)x + 2(b-d)y + r^2-s^2-(a^2+b^2)+(c^2+d^2) = 0
                    val a = constructible.timesN(constructible.sub(f1.p.x, f2.p.x), 2)
                    val b = constructible.timesN(constructible.sub(f1.p.y, f2.p.y), 2)
                    val c = constructible.sub(
                        constructible.sub(
                            constructible.pow(f1.r, 2),
                            constructible.pow(f2.r, 2)
                        ),
                        constructible.sub(
                            f1.p.squareNorm,
                            f2.p.squareNorm
                        )
                    )
                    LineCircleSolver.intersects(Line(a, b, c), f1)(
                        implicitly[LineLike[T] <:< Figure2D[T]],
                        ef1,
                        constructible,
                        ordering
                    )
                }
            }
        }
    }
}
