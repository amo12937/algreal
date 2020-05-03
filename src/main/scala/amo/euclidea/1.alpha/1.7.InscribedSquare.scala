package amo.euclidea.alpha

import amo.algreal.field.ConstructibleTrait
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    CircleCommandProvider,
    LineCommandProvider,
    VerticalBisectorCommandProvider
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

object Alpha_1_7_InscribedSquare {
    def problem[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Problem[T] = {
        implicit val nToT = constructible.fromInt _
        val p1: Point[T] = Point(0, 1)
        val p2: Point[T] = Point(0, 0)
        val c1: Circle[T] = Circle(p2, 1)

        val initialEnvironment = ProblemEnvironment.initialEnvironment[T](
            Board(Set(p1, p2), circles = Set(c1))
        )

        val q1: Point[T] = Point(-1, 0)
        val q2: Point[T] = Point(0, -1)
        val q3: Point[T] = Point(1, 0)

        val answer = new FullFillEnvironmentAnswer[T](
            Vector(
                Board(
                    lines = Set(
                        Line(p1, q1), Line(q1, q2), Line(q2, q3), Line(q3, p1)
                    )
                )
            ),
            Cost(6, 7)
        )

        val commands: Vector[CommandProvider[T]] = Vector(
            VerticalBisectorCommandProvider(),
            LineCommandProvider(),
            CircleCommandProvider(),
        )

        new Problem[T] {
            val problemL = ProblemL(initialEnvironment, commands, answer)
            val problemE = ProblemE(initialEnvironment, commands, answer)
        }
    }
}
