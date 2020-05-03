package amo.euclidea.alpha

import amo.algreal.field.ConstructibleTrait
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    GetCircleCommandProvider,
    GetLineCommandProvider,
}
import amo.geometry.figures.{ Point, Line, LineLike, LineSegment }
import amo.geometry.problems.{
    Board,
    Cost,
    FullFillEnvironmentAnswer,
    ProblemE,
    ProblemL,
    ProblemEnvironment
}

object Alpha_1_2_VerticalBisector {
    def problem[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Problem[T] = {
        implicit val nToT = constructible.fromInt _
        val p1: Point[T] = Point(0, 0)
        val p2: Point[T] = Point(2, 0)
        val l = LineSegment(p1, p2)
        val initialEnvironment = ProblemEnvironment.initialEnvironment[T](
            Board(Set(p1, p2), Set(l))
        )

        val goal: LineLike[T] = Line(Point(1, -1), Point(1, 1))
        val answer = new FullFillEnvironmentAnswer[T](
            Vector(Board(Set(), Set(goal))),
            Cost(3, 3)
        )

        val commands: Vector[CommandProvider[T]] = Vector(
            GetLineCommandProvider(),
            GetCircleCommandProvider()
        )

        new Problem[T] {
            val problemL = ProblemL(initialEnvironment, commands, answer)
            val problemE = ProblemE(initialEnvironment, commands, answer)
        }
    }
}

