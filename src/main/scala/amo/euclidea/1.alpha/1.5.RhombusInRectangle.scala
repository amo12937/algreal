package amo.euclidea.alpha

import amo.algreal.field.ConstructibleTrait
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    GetCircleCommandProvider,
    GetLineCommandProvider,
    GetVerticalBisectorCommandProvider
}
import amo.geometry.figures.{ Circle, Point, Line, LineLike, LineSegment }
import amo.geometry.problems.{
    Board,
    Cost,
    FullFillEnvironmentAnswer,
    ProblemE,
    ProblemL,
    ProblemEnvironment
}

object Alpha_1_5_RhombusInRectangle {
    def problem[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Problem[T] = {
        implicit val nToT = constructible.fromInt _
        val a = nToT(3)
        val p1: Point[T] = Point(0, 0)
        val p2: Point[T] = Point(a, 0)
        val p3: Point[T] = Point(a, 1)
        val p4: Point[T] = Point(0, 1)
        val l1: LineLike[T] = LineSegment(p1, p2)
        val l2: LineLike[T] = LineSegment(p2, p3)
        val l3: LineLike[T] = LineSegment(p3, p4)
        val l4: LineLike[T] = LineSegment(p4, p1)

        val initialEnvironment = ProblemEnvironment.initialEnvironment[T](
            Board(Set(p1, p2, p3, p4), Set(l1, l2, l3, l4))
        )

        val q1 = constructible.divide(
            constructible.add(constructible.pow(a, 2), 1),
            constructible.timesN(a, 2)
        )
        val q2 = constructible.sub(a, q1)

        val answer = new FullFillEnvironmentAnswer[T](
            Vector(
                Board(lines = Set(
                    Line(p1, Point(q2, 1)),
                    Line(Point(q1, 0), p3)
                )),
                Board(lines = Set(
                    Line(p4, Point(q2, 0)),
                    Line(Point(q1, 1), p2)
                ))
            ),
            Cost(3, 5)
        )

        val commands: Vector[CommandProvider[T]] = Vector(
            GetVerticalBisectorCommandProvider(),
            GetLineCommandProvider(),
            GetCircleCommandProvider(),
        )

        new Problem[T] {
            val problemL = ProblemL(initialEnvironment, commands, answer)
            val problemE = ProblemE(initialEnvironment, commands, answer)
        }
    }
}
