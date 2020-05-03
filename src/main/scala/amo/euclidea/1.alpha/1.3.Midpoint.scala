package amo.euclidea.alpha

import amo.algreal.field.ConstructibleTrait
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    CircleCommandProvider,
    LineCommandProvider,
    VerticalBisectorCommandProvider
}
import amo.geometry.figures.{ Point }
import amo.geometry.problems.{
    Board,
    Cost,
    FullFillEnvironmentAnswer,
    ProblemE,
    ProblemL,
    ProblemEnvironment
}

object Alpha_1_3_Midpoint {
    def problem[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Problem[T] = {
        implicit val nToT = constructible.fromInt _
        val p1: Point[T] = Point(0, 0)
        val p2: Point[T] = Point(2, 0)
        val initialEnvironment = ProblemEnvironment.initialEnvironment[T](
            Board(Set(p1, p2))
        )

        val goal: Point[T] = Point(1, 0)
        val answer = new FullFillEnvironmentAnswer[T](
            Vector(Board(Set(goal))),
            Cost(2, 4)
        )

        val commands: Vector[CommandProvider[T]] = Vector(
            LineCommandProvider(),
            CircleCommandProvider(),
            VerticalBisectorCommandProvider()
        )

        new Problem[T] {
            val problemL = ProblemL(initialEnvironment, commands, answer)
            val problemE = ProblemE(initialEnvironment, commands, answer)
        }
    }
}

